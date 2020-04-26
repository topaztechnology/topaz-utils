package com.topaz.utils

import java.util.UUID

trait StringPimps {

  def localNewline: String = StringUtils.localNewline

  implicit class PimpedString(s: String){

    def indent(nSpaces: Int): String = {
      s.split(localNewline).map{
        line =>
          " " * nSpaces + line
      }.mkString(localNewline)
    }

    def prefix(t: String): String = {
      s.split(localNewline).map(l => t + l).mkString(localNewline)
    }

    def randomize(): String = {
      s"$s-${UUID.randomUUID().toString.take(10)}"
    }
  }

}

