package com.topaz.utils

import java.io._
import java.security.SecureRandom
import java.util.concurrent.atomic.AtomicBoolean

trait FileTestUtils extends FileUtils {

  private def topazTempDirectory(): File = {
    var mkdirs: Boolean = false
    var tempDir: File = null
    var n: Int = 0
    do {
      tempDir = new File(systemTempDirectory, "topazTmp" + FileTestUtils.ulongString())
      mkdirs = tempDir.mkdirs()
      n += 1
    } while (!mkdirs && n < 10)
    require(mkdirs, s"Failed to create directory ${tempDir.getAbsolutePath} after $n attempts")
    tempDir
  }

  def withTempDir[A](f: File => A): A = {
    val dir = topazTempDirectory()
    try {
      f(dir)
    }finally {
      dir.recursiveDelete()
    }
  }

  def withTempDir2[A](f: (File, File) => A): A = {
    val (dir1, dir2) = (topazTempDirectory(), topazTempDirectory())
    try {
      f(dir1, dir2)
    } finally {
      dir1.recursiveDelete()
      dir2.recursiveDelete()
    }
  }

  def withTempDir3[A](f: (File, File, File) => A): A = {
    val (dir1, dir2, dir3) = (topazTempDirectory(), topazTempDirectory(), topazTempDirectory())
    try {
      f(dir1, dir2, dir3)
    } finally {
      dir1.recursiveDelete()
      dir2.recursiveDelete()
      dir3.recursiveDelete()
    }
  }

  /**
    * Intended to be swapped in for withTempDir as a convenience
    * when analysing broken unit test
    */
  private val testDirBeingUsed = new AtomicBoolean(false)
  def withTestDir[A](f: File => A) = {
    assert(testDirBeingUsed.compareAndSet(false, true), "Test directory used by another test - this will end badly")
    val dir = file("/tmp/unit-test-temp")
    val mkdir = dir.mkdir()
    require(mkdir, s"Failed to create directory ${dir.getAbsolutePath}")
    dir.listFiles.foreach(_.recursiveDelete())
    f(dir)
  }

  def withFileReader[T](file : File)(f : BufferedReader => T) = {
    createParentDir(file)
    val in = new BufferedReader(new FileReader(file))
    try {
      f(in)
    } finally {
      in.close()
    }
  }

  def withFileAppender(file : File)(f : BufferedWriter => _){
    createParentDir(file)
    val fstream = new FileWriter(file, true)
    val out = new BufferedWriter(fstream)
    try {
      f(out)
    } finally {
      out.close()
    }
  }

  def withFileLineReader[A](file : File)(f : String => A) = {
    var res : List[A] = Nil
    if (file.exists()) {
      withFileReader(file){
        in : BufferedReader =>
          var line = in.readLine
          while (line != null) {
            res = f(line) :: res
            line = in.readLine
          }
      }
    }
    res.reverse
  }
}

object FileTestUtils extends FileTestUtils {
  val random = new SecureRandom()

  def ulongString(): String = java.lang.Long.toUnsignedString(random.nextLong())
}
