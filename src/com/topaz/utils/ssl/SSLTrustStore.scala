package com.topaz.utils.ssl

import java.io.{File, FileInputStream}
import java.security.{KeyStore, SecureRandom}

import com.topaz.utils.Log
import javax.net.ssl._

sealed trait SSLTrustStore {
  protected def keyStore: KeyStore

  def keyManagers: Array[KeyManager]

  private def trustManagers: Array[TrustManager] = {
    val trustManagerFactory = TrustManagerFactory.getInstance("SunX509")
    trustManagerFactory.init(keyStore)
    trustManagerFactory.getTrustManagers
  }

  def sslContext: SSLContext = {
    val sslContext = SSLContext.getInstance("TLS")
    sslContext.init(keyManagers, trustManagers, new SecureRandom)
    sslContext
  }
}

case object DefaultSSLTrustStore extends SSLTrustStore {
  protected def keyStore: KeyStore = null // Use default

  def keyManagers: Array[KeyManager] = null // Use default
}

case class SpecifiedSSLTrustStore(keyStoreFile: File, maybePassword: Option[String]) extends SSLTrustStore with Log {
  require(keyStoreFile.exists(), s"Key store file not found: ${keyStoreFile.getAbsolutePath}")
  val password: Array[Char] = maybePassword.map(_.toCharArray).getOrElse(SSLUtils.NULL_PASSWORD)

  protected def keyStore: KeyStore = {
    val ks = KeyStore.getInstance("PKCS12")
    ks.load(new FileInputStream(keyStoreFile), password)
    log.debug(s"Loaded key store from ${keyStoreFile.getAbsolutePath}")
    ks
  }

  def keyManagers: Array[KeyManager] = {
    val keyManagerFactory = KeyManagerFactory.getInstance("SunX509")
    keyManagerFactory.init(keyStore, password)
    keyManagerFactory.getKeyManagers
  }
}
