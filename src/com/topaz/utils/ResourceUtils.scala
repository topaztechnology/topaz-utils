package com.topaz.utils

import java.io.{File, FileOutputStream}
import java.net.URLDecoder
import java.util.jar.JarFile

import scala.collection.JavaConverters._
import scala.io.Source

trait ResourceUtils extends FileUtils {
  def withSourceFromResource[R](resource: String)(f: Source => R): R = {
    val resourceStr = getClass.getResource(resource)
    if (resourceStr == null) {
      throw new NullPointerException(s"Failed to read resource $resource")
    }
    val source = Source.fromURL(resourceStr)
    try {
      f(source)
    } finally {
      source.close()
    }
  }

  def readLinesFromResource(resource: String): List[String] = {
    withSourceFromResource(resource)(_.getLines().toList)
  }

  def stringFromResource(resource: String): String = {
    withSourceFromResource(resource)(_.mkString)
  }
  
  def resourceExists(resource: String): Boolean = getClass.getResource(resource) != null

  def copyResourceToFile(resourceLocation: String, file: File): Unit = {
    val classLoader = getClass.getClassLoader

    val buffer = new Array[Byte](1024)
    using(classLoader.getResourceAsStream(resourceLocation)) {
      is =>
        using(new FileOutputStream(file)) {
          os =>
            while (is.available() > 0) {
              val read = is.read(buffer, 0, buffer.length)
              os.write(buffer, 0, read)
            }
        }
    }
  }

  def withFileFromResource[T](resource: String)(fn: File => T): T =
    withTempFile { tempFile => {
      copyResourceToFile(resource, tempFile)
      fn(tempFile)
    }
  }

  def resourceFilenamesFromPath(path: String): Seq[String] = {
    val dirURL = getClass.getResource(path)

    if (dirURL == null)
      Seq()
    else {
      dirURL.getProtocol match {
        case "file" => new File(dirURL.toURI).listFiles().map(_.getName)
        case "jar" => {
          val jarPath = dirURL.getPath.substring(5, dirURL.getPath.indexOf("!"))
          val jar = new JarFile(URLDecoder.decode(jarPath, "UTF-8"))
          val jarEntries = jar.entries().asScala.toSeq
          val pathWithoutLeadingSlash = if (path(0) == '/') path.drop(1) else path
          jarEntries.map(_.getName)
            .filter(_.startsWith(pathWithoutLeadingSlash))
            .map(_.drop(pathWithoutLeadingSlash.length + 1))
        }
      }
    }
  }
}

object ResourceUtils extends ResourceUtils
