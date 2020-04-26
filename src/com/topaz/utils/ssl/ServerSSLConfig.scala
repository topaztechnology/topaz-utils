package com.topaz.utils.ssl

import java.io.File

import com.topaz.utils.FileUtils
import com.typesafe.config.Config

sealed trait ServerSSLConfig {
  def isSSLEnabled: Boolean
  def protocol: String = if (isSSLEnabled) "https" else "http"
}

sealed trait ServerSSLEnabled extends ServerSSLConfig with FileUtils {
  val isSSLEnabled: Boolean = true
  val storeDirectory: File
  val keyStoreFilename: String
  val passwordFilename: String

  def keyStoreFile: File = file(storeDirectory, keyStoreFilename)
  def passwordFile = file(storeDirectory, passwordFilename)
  def password: String = readLines(passwordFile).head
}

object ServerSSLConfig extends FileUtils {
  case object Disabled extends ServerSSLConfig {
    def isSSLEnabled: Boolean = false
  }

  case class Provided(
    storeDirectory: File,
    keyStoreFilename: String,
    passwordFilename: String
  ) extends ServerSSLEnabled

  case class Generated(
    storeDirectory: File,
    keyStoreFilename: String,
    passwordFilename: String,
    generatedConf: Config
  ) extends ServerSSLEnabled {
    def caFile = file(storeDirectory, SSLUtils.TOPAZ_CA_FILENAME)
  }

  def apply(sslConfig: Config): ServerSSLConfig = {

    sslConfig.getString("status") match {
      case "disabled" => Disabled
      case "provided" => Provided(
        file(sslConfig.getString("directory")),
        sslConfig.getString("keystore-filename"),
        sslConfig.getString("password-filename")
      )
      case "generated" => Generated(
        file(sslConfig.getString("directory")),
        sslConfig.getString("keystore-filename"),
        sslConfig.getString("password-filename"),
        sslConfig.getConfig("generated")
      )
      case other => throw new Exception(s"Invalid SSL status: $other, proceeding with SSL disabled")
    }
  }
}
