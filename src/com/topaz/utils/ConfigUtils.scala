package com.topaz.utils

import java.io.File
import java.net.URL
import java.util.{Enumeration => JEnumeration}

import com.typesafe.config.impl.Parseable
import com.typesafe.config.{Config, ConfigFactory, ConfigParseOptions}

import scala.collection.JavaConverters._

class ConfigClassLoader(loader: ClassLoader) extends ClassLoader(loader) {
  override def getResources(name: String): JEnumeration[URL] = {
    val resources = super.getResources(name).asScala.toList
    // Ensure topaz libs come at the beginning of the classpath, so our config will override any other libraries
    val (topaz, other) = resources.partition { url =>
      // We need to check if we are running in Intellij or from a fullpack release.
      // If we are running from Intellij our classes will be in the target directory otherwise
      // they'll be in a jar with the 'topaz-' prefix.
      if (url.getProtocol == "jar") {
        url.getPath.matches(""".*\/topaz.*\.jar!\/.*""")
      } else {
        url.getPath.contains("/target/")
      }
    }
    java.util.Collections.enumeration((topaz ++ other).asJava)
  }
}

object ConfigUtils {
  /*
    This allows us to parse config without eagerly resolving references. This is a common problem
    for Akka persistence where journal databases are referred to using references, but they get resolved
    too early, and subsequent database config overrides have no effect as they are not re-resolved
   */
  private def parseConfig() = {
    val loader = new ConfigClassLoader(ConfigUtils.getClass.getClassLoader)
    Parseable
      .newResources("reference.conf", ConfigParseOptions.defaults().setClassLoader(loader))
      .parse()
      .toConfig
  }

  def loadWithoutResolve(): Config =
    ConfigFactory.defaultOverrides()
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(parseConfig())

  def loadWithoutResolve(conf: Config): Config =
    ConfigFactory.defaultOverrides()
      .withFallback(conf)
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(parseConfig())

  def loadWithoutResolve(confFile: File): Config = loadWithoutResolve(ConfigFactory.parseFile(confFile))
}
