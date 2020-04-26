package com.topaz.utils

import java.net.InetAddress

object NetworkTestUtils {
  def freePort(): Int = {
    NetworkUtils.findFreePorts(1).head
  }

  def hostAddress: String = InetAddress.getLocalHost.getHostAddress
}
