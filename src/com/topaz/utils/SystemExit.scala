package com.topaz.utils

object SystemExit extends Log {

  /**
    * A sys.exit that doesn't print out all this information can be very painful to track down.
    */
  def apply(status: Int, message: String, t: Throwable = null): Nothing = {
    Option(t).foreach(_.printStackTrace())

    val newline = StringUtils.localNewline
    val fullMsg = s"Exiting. Reason: '$message'.$newline" +
      s"Called from$newline" + new Throwable().getStackTrace.toVector.slice(1, 10).mkString(newline)
    log.info(fullMsg)
    
    System.out.println(fullMsg)

    System.out.flush()
    System.err.flush()

    sys.exit(status)
  }
}
