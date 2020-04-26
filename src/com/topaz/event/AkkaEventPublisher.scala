package com.topaz.event

import akka.actor.ActorSystem

class AkkaEventPublisher()(implicit system: ActorSystem) extends EventPublisher {
  def publish(event: Event): Unit = system.eventStream.publish(event)
}

