package com.topaz.utils

import java.io.StringReader

import com.github.tototoshi.csv.CSVReader

import scala.io.Source

trait CSVUtils {
  def csvReaderFromString(csv: String): CSVReader = {
    CSVReader.open(new StringReader(csv))
  }

  def csvReaderFromResource(resource: String): CSVReader = {
    val resourceStr = getClass.getResource(resource)
    if (resourceStr == null) {
      throw new NullPointerException(s"Failed to read resource $resource")
    }

    CSVReader.open(Source.fromURL(resourceStr))
  }
}

object CSVUtils extends CSVUtils
