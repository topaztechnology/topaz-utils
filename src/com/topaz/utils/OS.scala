package com.topaz.utils

import scala.util.control.NonFatal

sealed trait OS {
  def windows: Boolean
}

case object OSX extends OS {
  override def windows = false
}

case object WindowsXP extends OS {
  def windows: Boolean = true
}

case object Windows7 extends OS {
  def windows: Boolean = true
}

case object WindowsUnknown extends OS {
  def windows: Boolean = true
}

case object Linux extends OS {
  override def windows = false
}

case object UnknownOS extends OS {
  def windows: Boolean = false
}

object OS {
  lazy val os: OS = {
    val osName = System.getProperty("os.name").toLowerCase
    if (osName.startsWith("mac"))
      OSX
    else if (osName.startsWith("linux"))
      Linux
    else if (osName.startsWith("windows"))
      WindowsUnknown
    else
      UnknownOS
  }

  def isWindows: Boolean = os.windows
}

object OSUtils {
  def isLinux: Boolean = OS.os == Linux

  def usernameAndDomain: (String, Option[String]) = {
    val username = sys.props("user.name")

    if (OS.isWindows) {
      try {
        val clazz = Class.forName("com.sun.security.auth.module.NTSystem")
        val ntSystem = clazz.getDeclaredConstructor().newInstance()
        val getNameMethod = clazz.getMethod("getName")
        val getDomainMethod = clazz.getMethod("getDomain")
        val name = getNameMethod.invoke(ntSystem).asInstanceOf[String]
        val domain = getDomainMethod.invoke(ntSystem).asInstanceOf[String]

        name -> Some(domain)
      } catch {
        case NonFatal(_) =>
          username -> None
      }
    } else {
      username -> None
    }
  }
}
