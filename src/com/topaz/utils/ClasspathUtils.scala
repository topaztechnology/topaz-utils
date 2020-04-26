package com.topaz.utils

import java.io.File

object ClasspathUtils extends FileUtils {

  def classpathEntries: Seq[String] = {
    val classpath = System.getProperty("java.class.path")
    val classpathEntries = classpath.split(File.pathSeparator)

    classpathEntries.toSeq
  }

  /**
    * Returns all implementations of a given class/trait in the current classpath.
    * Used to ensure that we have samples of each (e.g.) instrument/market data type
    * for testing
    */
  def implementationsOfBaseClass(baseClass: Class[_]): Set[Class[_]] = {
    val classes :Seq[Class[_]] = ClasspathUtils.classpathEntries.map(new File(_)).filter{
      file => file.isDirectory 
    }.flatMap{ 
      directory =>

        val possibleClassNames: Seq[String] = FileUtils.classFiles(directory).filterNot{
          classfile => classfile.getName.contains('$')
        }.map(_.className(directory)).distinct

        possibleClassNames.flatMap { 
          className: String =>
            val class_ = Class.forName(className)

            val maybeClass: Option[Class[_]] = {
              if (!class_.isInterface && baseClass.isAssignableFrom(class_)) {
                Some(class_)
              } else 
                None
            }

            maybeClass
        }
    }

    classes.toSet
  }
}

