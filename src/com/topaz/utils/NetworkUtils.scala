package com.topaz.utils

import java.net.{InetAddress, ServerSocket}

import com.typesafe.config.Config

object NetworkUtils {
  def findFreePorts(howMany: Int): Seq[Int] = {
    val sockets = (0 until howMany).map {
      _ =>
        new ServerSocket(0)
    }
    val ports = sockets.map(_.getLocalPort)
    sockets.foreach(_.close)
    ports
  }

  def akkaHostname(conf: Config): String = {
    conf.getString("akka.remote.artery.canonical.hostname") match {
      case "<getHostAddress>" => InetAddress.getLocalHost.getHostAddress
      case "<getHostName>" => InetAddress.getLocalHost.getHostName
      case o => o
    }
  }
  
  def akkaPort(conf: Config): Int = {
    conf.getInt("akka.remote.artery.canonical.port")
  }
  
  def akkaManagementHostname(conf: Config): String = {
    conf.getString("akka.management.http.hostname") match {
      case "<hostname>" => InetAddress.getLocalHost.getHostAddress
      case "" => InetAddress.getLocalHost.getHostAddress
      case o => o
    }
  }
  
  def akkaManagementPort(conf: Config): Int = {
    conf.getInt("akka.management.http.port")
  }
}
