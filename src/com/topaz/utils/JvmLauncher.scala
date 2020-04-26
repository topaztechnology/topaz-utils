package com.topaz.utils

import java.io.File
import java.lang.management.ManagementFactory

import com.topaz.utils.IOUtils.IOThreadPool

import scala.collection.JavaConverters._
import scala.concurrent.Future

object JvmLauncher {
  private lazy val javaHome = sys.props("java.home")

  def fork(mainClass: String, extraJvmArgs: Seq[String], args: Seq[String], inheritOutput: Boolean): Jvm = {
    val runtimeMxBean = ManagementFactory.getRuntimeMXBean
    // we create the new jvm with the same jvm args as this process. 
    // (we have to remove agentlib as that binds to a port)
    val jvmArgs = runtimeMxBean.getInputArguments.asScala.filterNot(_.contains("agentlib")) ++ extraJvmArgs
    
    val classPath: Seq[String] = ClasspathUtils.classpathEntries
    launch(new File("."), jvmArgs, classPath, mainClass, args, inheritOutput)
  }
  
  def launch(workingDirectory: File, jvmArgs: Seq[String], classPath: Seq[String], 
             mainClass: String, args: Seq[String], inheritOutput: Boolean): Jvm = {

    val javaProc = Seq(javaHome, "bin", "java") mkString sys.props("file.separator")
    val pb: ProcessBuilder = new ProcessBuilder((Seq(javaProc) ++ jvmArgs ++ Seq(mainClass) ++ args).asJava)
    
    val env = pb.environment()
    env.put("CLASSPATH", classPath.distinct mkString File.pathSeparator)

    pb.directory(workingDirectory)
    
    pb.redirectInput(ProcessBuilder.Redirect.INHERIT)
    if(inheritOutput) {
      pb.redirectError(ProcessBuilder.Redirect.INHERIT)
      pb.redirectOutput(ProcessBuilder.Redirect.INHERIT)
    }

    new Jvm(pb)
  }
  
}

class Jvm(pb: ProcessBuilder) {
  def run(pool: IOThreadPool): Future[Process] = {
    pool.executeBlockingIO {
      val proc = pb.start()
      proc
    }
  }
}
