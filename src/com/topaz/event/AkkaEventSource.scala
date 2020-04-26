package com.topaz.event

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import akka.NotUsed
import akka.actor.{ActorPath, ActorSystem}
import akka.stream.scaladsl.{Keep, Merge, Sink, Source}
import akka.stream.{CompletionStrategy, OverflowStrategy}
import com.topaz.utils.{Log, RichConcurrent}

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

object AkkaEventSource extends RichConcurrent with Log {
  // We get bursts of events (particularly when writing market data) so the queue can grow quite large 
  // before it is flushed to the listener.
  val MaxQueueSize = 10000

  private val subscriptions = new AtomicReference[Map[String, Vector[ActorPath]]](Map.empty)

  def subscriptionsCount: Map[String, Int] = subscriptions.get().mapValues(_.size)

  /**
    * @return The source and a function to call to close the source when done.
    */
  def eventSource(name: String)(implicit system: ActorSystem): (Source[Event, NotUsed], () => Unit) = {
    val completionMatcher: PartialFunction[Any, CompletionStrategy] = {
      case akka.actor.Status.Success(s: CompletionStrategy) => s
      case akka.actor.Status.Success(_) => CompletionStrategy.draining
      case akka.actor.Status.Success => CompletionStrategy.draining
    }
    val failureMatcher: PartialFunction[Any, Throwable] = {
      case akka.actor.Status.Failure(cause) => cause
    }
    val (actorRef, publisher) = Source.actorRef[Event](
      completionMatcher, failureMatcher, MaxQueueSize, OverflowStrategy.fail
    ).toMat(Sink.asPublisher(false))(Keep.both).run()
    system.eventStream.subscribe(actorRef, classOf[Event])
    val actorPath = actorRef.path
    val state = subscriptions.transformAndGet {
      map =>
        map + (name -> (map.getOrElse(name, Vector()) :+ actorPath))
    }
    def stateStr(state: Map[String, Vector[ActorPath]]) = state.mapValues(_.size).mkString(", ")

    log.trace(s"Current subscriptions for eventSource (new $name): ${stateStr(state)}")

    def close(): Unit = {
      system.eventStream.unsubscribe(actorRef)
      // Source.actorRef cannot be completed by using ActorSystem.stop anymore #27321
      // https://github.com/akka/akka/issues/27321
      actorRef ! akka.actor.Status.Success(akka.stream.CompletionStrategy.immediately)

      val state = subscriptions.transformAndGet {
        map =>
          map.getOrElse(name, Vector()).filterNot(_ == actorPath) match {
            case Vector() =>
              map - name
            case remaining =>
              map + (name -> remaining)
          }
      }
      log.trace(s"Current subscriptions for eventSource (del $name): ${stateStr(state)}")
    }

    Source.fromPublisher(publisher) -> (close _)
  }

  /**
    * If you want the event source for the life of the Jvm then this is handy. Otherwise
    * you'll need to close it when you're done and should be using `eventSource` above.
    */
  def eventSourceNoClose(name: String)(implicit system: ActorSystem): Source[Event, NotUsed] = {
    eventSource(name)._1
  }

  trait AkkaEventSourceReceiver {
    type ReceiveEvent = PartialFunction[Event, Unit]

    def receive: ReceiveEvent

    def shouldCloseSource(): Boolean
  }

  def subscribe(name: String, r: AkkaEventSourceReceiver)(implicit system: ActorSystem): () => Unit = {
    val (source, close) = eventSource(name)

    // Closing the source is asynchronous. So when someone asks to close it we record it in an atomic bool
    // and stop sending them events.
    val sourceClosed = new AtomicBoolean(false)
    val tickTimer = FiniteDuration(100, "ms")
    val tick = Source.tick(tickTimer, tickTimer, NoOp)
    val combinedSource = Source.combine(source, tick)(Merge(_, eagerComplete = true))
    combinedSource.runForeach(
      e =>
        if (sourceClosed.get()) {
          // We can still receive a few events after closing - skip
          log.trace(s"Still receiving in closed source: $name")
        } else if (r.shouldCloseSource()) {
          log.trace(s"Closing source $name")
          sourceClosed.set(true)
          close()
        } else {
          e match {
            case NoOp =>
            case _ =>
              try {
                if (r.receive.isDefinedAt(e))
                  r.receive(e)
              } catch {
                case NonFatal(ex) =>
                  log.warn(s"You should not be throwing in the handle method: $e", ex)
              }
          }
        }
    )
    close
  }
}
