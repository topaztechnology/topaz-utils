package com.topaz.utils

import java.util.TimerTask
import java.util.concurrent.{Executors, TimeUnit}

import com.jezhumble.javasysmon.JavaSysMon

import scala.util.control.NonFatal

object PID extends Log {
  def apply(): Int = {
    val sysMon = new JavaSysMon()
    sysMon.currentPid()
  }
  
  def parent: Int = {
    val sysMon = new JavaSysMon()
    val pid = sysMon.currentPid()
    
    val processTree = sysMon.processTree()
    val info = processTree.find(pid).processInfo
    info.getParentPid
  }

  def callbackOnParentProcessDeath(parentPID: Int, f: () => Unit): Unit = {
    log.info(s"Monitoring parent PID: $parentPID")
    val scheduler = Executors.newScheduledThreadPool(1)
    scheduler.scheduleAtFixedRate(new TimerTask {
      override def run(): Unit = {
        try {
          val currentParentId = PID.parent
          log.trace(s"Current parent PID: $currentParentId, initial PID: $parentPID")
          // if the parent process has died
          if (currentParentId != parentPID) {
            try {
              f()
            } catch {
              case NonFatal(e) =>
                log.error("Failed to run method f() on parent death", e)
            }
          }
        } catch {
          case NonFatal(e) =>
            log.error("Failed to find process info", e)
        }

      }
    }, 10, 10, TimeUnit.SECONDS)
  }
}

