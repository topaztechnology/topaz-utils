package com.topaz.utils

import java.io._
import java.nio.file.Paths

import org.apache.commons.io.FilenameUtils
import org.apache.commons.io.output.TeeOutputStream

import scala.collection.immutable
import scala.io.Codec
import scala.language.reflectiveCalls

trait FileUtils {
  val codec: Codec = Codec.UTF8
  lazy val systemTempDirectory: File = {
    if (OS.os == OSX) {
      // OSX uses tmp directories like /var/folders/7j/w56l62zs3j7gm9h95nyqj6wh02300gn/T/ to help
      // improve security but it makes it a pain to map into docker containers for testing.
      new File("/tmp/")
    } else {
      new File(sys.props("java.io.tmpdir"))
    }
  }

  implicit class RichFile(plainFile: File) {

    def absPath: String = plainFile.getAbsolutePath

    def asAbsoluteFile = {
      val absFile = file(plainFile.getAbsolutePath)
      require(absFile.isAbsolute, s"$absFile should be absolute")
      absFile
    }

    def relativeTo(dir: File): File = {
      val path = Paths.get(plainFile.getCanonicalPath)
      val base = Paths.get(dir.getCanonicalPath)
      base.relativize(path).toFile
    }

    def className(classDirectory: File): String = {
      val relativeFile = relativeTo(classDirectory)
      relativeFile.getPath.split('.').head.replace(File.separator, ".")
    }

    def readLines: List[String] = {
      if (plainFile.exists) {
        val source = scala.io.Source.fromFile(plainFile)
        try source.getLines().toList
        finally source.close()
      }
      else Nil
    }

    def touch = {
      if (plainFile.exists) {
        if (!plainFile.setLastModified(System.currentTimeMillis))
          throw new Exception("Could not touch file " + plainFile)
      } else {
        createParentDir(plainFile)
        plainFile.createNewFile()
      }
      assert(plainFile.exists, plainFile + "should exist")
      plainFile
    }

    def recursiveDelete() {
      if (plainFile.exists && plainFile.isDirectory) {
        Option(plainFile.listFiles).foreach(fs => fs.foreach(_.recursiveDelete()))
        plainFile.delete
      } else
        plainFile.delete // TODO Will silently fail on Windows with memory mapped files as lock persists until process dies
    }

    def recreate(): Unit = {
      if (plainFile.exists())
        plainFile.recursiveDelete()
      plainFile.mkdirs()
    }

    def absolutePathWithForwardSlashes: String = {
      plainFile.getAbsolutePath.replace('\\', '/')
    }
  }

  def currentDirectory: File = new File(sys.props("user.dir"))

  def file(f: File, d: String*): File = {
    require(f != null, "Null file used")  // Because Java will happily ignore a null f
    d.toList match {
      case Nil => f
      case x::rest => file(new File(f, x), rest: _*)
    }
  }

  def file(f: String, d: String*): File = {
    require(f != null && !d.contains(null), "Constructing a file with null parameters")
    file(new File(f), d: _*)
  }

  def listFiles(dir: File, filter: String => Boolean): immutable.IndexedSeq[File] ={
    dir.listFiles(new FilenameFilter {
      def accept(dir: File, name: String): Boolean = filter(name)
    }).toIndexedSeq
  }


  def findFiles(pred: File => Boolean, maxDepth: Int, dirs: File*): Seq[File] = {
    def rec(file: File, depth: Int): Seq[File] = {
      if (maxDepth > 0 && depth > maxDepth) {
        Vector()
      } else {
        if (file.isDirectory)
          file.listFiles.distinct.flatMap(rec(_, depth + 1))
        else if (pred(file))
          Vector(file)
        else
          Vector()
      }
    }
    dirs.flatMap(rec(_, depth = 1)).map{f: File => f.asAbsoluteFile}
  }


  def findFilesWithExtension(ext: String, maxDepth: Int, dirs: File*): Seq[File] =
    findFiles(_.getName.endsWith("." + ext), maxDepth, dirs: _*)

  def classFiles(dirs: File*): Seq[File] = findFilesWithExtension("class", maxDepth = -1, dirs: _*)

  def createParentDir(file: File) {
    Option(file.getParentFile).filterNot(_.exists).foreach(_.mkdirs)
  }

  /**
   * Don't want to use PrintWriter as that swallows exceptions
   */
  implicit class RichBufferedWriter(writer: BufferedWriter) {
    def println(text: String) {
      writer.write(text)
      writer.newLine()
    }
    def writeTabSeparatedLine(terms: String*): Unit = {
      writer.write(terms.mkString("", "\t", StringUtils.localNewline))
      writer.flush()
    }
  }

  def createFileWriter(file: File, append: Boolean, maybeTeeStream: Option[OutputStream] = None): BufferedWriter = {
    val fileStream = new FileOutputStream(file, append)
    val stream = maybeTeeStream match {
      case Some(s) => new TeeOutputStream(fileStream, s)
      case None => fileStream
    }
    new BufferedWriter(new OutputStreamWriter(stream, codec.charSet))
  }

  def withFileWriter(file: File, append: Boolean, maybeTeeStream: Option[OutputStream] = None)(f: BufferedWriter => _) {
    createParentDir(file)
    val out = createFileWriter(file, append, maybeTeeStream)
    try {
      f(out)
    } finally {
      out.close()
    }
  }

  def recreateDir(dir: File): Unit = dir.recreate()

  def mkdirs(dir: File, subdirs: String*): File = {
    val dir_ = file(dir, subdirs: _*)
    dir_.mkdirs
    dir_
  }

  def readLines(file: File): List[String] = {
    val source = scala.io.Source.fromFile(file)(codec)
    try source.getLines().toList
    finally source.close()
  }
  
  def stringFromFile(file: File): String = {
    readLines(file).mkString
  }

  def readSingleLineFile(file: File): String = {
    readLines(file) match {
      case Seq(line) => line
      case line +: others if others.forall(_.trim.isEmpty) => line
      case o => throw new Exception(s"Expected a single line file, got a ${o.size} line file.")
    }
  }

  def foreachLine(file: File)(f: String => Unit): Unit = {
    val source = scala.io.Source.fromFile(file)(codec)
    try source.getLines().foreach(f)
    finally source.close()
  }

  def createFile(parent: File, name: String): File = {
    val file = new File(parent, name)
    file.getParentFile.mkdirs()
    file.createNewFile()
    file
  }

  def writeToFile(file: File, text: String, append: Boolean): File = {
    withFileWriter(file, append) {
      out: BufferedWriter =>
        out.write(text)
    }
    file
  }

  def safeListFiles(file: File): List[File] = Option(file.listFiles).toList.flatten
  def subDirs(file: File): List[File] = safeListFiles(file).filter(_.isDirectory)

  private def withFileThenDelete[A](file: File, f: File => A): A = {
    try {
      f(file)
    } finally {
      file.recursiveDelete()
    }
  }

  def withTempFile[A](f: File => A): A = {
    val file = File.createTempFile(System.nanoTime.toString, null, systemTempDirectory)
    withFileThenDelete(file, f)
  }

  def withTempFiles[A](n: Int)(f: Seq[File] => A): A = {
    val files = Vector.fill(n)(File.createTempFile(System.nanoTime.toString, null, systemTempDirectory))
    try {
      f(files)
    } finally {
      files.foreach(_.recursiveDelete())
    }
  }

  def withTempFileIn[A](directory: File)(f: File => A): A = {
    val file = File.createTempFile(System.nanoTime.toString, null, directory)
    withFileThenDelete(file, f)
  }


  def using[X <: {def close()}, A](resource : X)(f : X => A): A = {
    require(resource != null, "Null resource")
    try {
      f(resource)
    } finally {
      resource.close()
    }
  }

  def escapedAbsolutePath(f: File): String = {
    // Required for Windows paths
    f.getAbsolutePath.replace("""\""", """\\""")
  }

  def extension(fileName: String): String = FilenameUtils.getExtension(fileName)
}

object FileUtils extends FileUtils
