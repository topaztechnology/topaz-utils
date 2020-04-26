package com.topaz

/**
  * For errors that 'shouldn't happen', and that we think have been 
  * caused by a programming error - rather than user error, or missing/invalid data 
  * 
  * From the javadocs for Exception:
  * @param cause the cause.  (A {@code null} value is permitted,
  * and indicates that the cause is nonexistent or unknown.)
  */
case class TopazCodingError(msg: String, cause: Throwable = null) extends Exception(msg, cause)

object TopazCodingError {
  def require(predicate: => Boolean, msg: => String) {
    if (! predicate)
      throw TopazCodingError(msg)
  }

  def requireAll[T](s: Iterable[T])(check: T => Boolean, message: T => String): Unit = {
    s.foreach {
      e =>
        require(check(e), message(e))
    }
  }
}
