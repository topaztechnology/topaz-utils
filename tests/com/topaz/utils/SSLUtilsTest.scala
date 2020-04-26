package com.topaz.utils

import java.security.cert.X509Certificate
import java.util.{Collection => JCollection}

import com.topaz.utils.ssl.{SSLUtils, ServerSSLConfig, SpecifiedSSLTrustStore}
import com.typesafe.config.ConfigFactory
import javax.net.ssl.SSLContext
import org.scalatest.tags.Slow

import scala.collection.JavaConverters._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

@Slow
class SSLUtilsTest extends AnyFreeSpec with Matchers with FileTestUtils {

  // Unfortunately there seems to be no supported way to extract certs from an SSLContext
  private def trustedCertsFromSSLContext(sslContext: SSLContext): List[X509Certificate] = {
    val contextSpiField = sslContext.getClass.getDeclaredField("contextSpi")
    contextSpiField.setAccessible(true)
    val contextSpi = contextSpiField.get(sslContext)

    val x509TrustManagerField = contextSpi.getClass.getSuperclass.getSuperclass.getSuperclass.getDeclaredField("trustManager")
    x509TrustManagerField.setAccessible(true)
    val x509TrustManager = x509TrustManagerField.get(contextSpi)

    val trustedCertsField = x509TrustManager.getClass.getDeclaredField("trustedCerts")
    trustedCertsField.setAccessible(true)
    trustedCertsField.get(x509TrustManager).asInstanceOf[JCollection[X509Certificate]].asScala.toList
  }

  "Can create a valid SSL context from no initial keystores" in {
    withTempDir { keyStoreDirectory =>
      val sslConfig = ConfigFactory.parseString(
        s"""
          |status = generated
          |directory = "${keyStoreDirectory.getAbsolutePath}"
          |keystore-filename = "server.p12"
          |password-filename = "server-password"
          |
          |generated {
          |  signature-algorithm = "SHA512withRSA"
          |  distinguished-name-suffix = "O=Topaz, L=London, C=UK"
          |
          |  certificate-authority {
          |    name = "Topaz CA"
          |    key-size = 4096
          |    validity = 9999
          |    keystore-filename = "topaz-ca-with-keys.p12"
          |    password-filename = "topaz-ca-password"
          |  }
          |
          |  server {
          |    name = "test.topaz.technology"
          |    key-size = 2048
          |    validity = 1825
          |  }
          |}
        """.stripMargin)

      val serverSSLConfig = ServerSSLConfig(sslConfig)
      SSLUtils.initialise(serverSSLConfig)

      // Test CA trust keystore
      val trustKeyStoreFile = file(keyStoreDirectory, "topaz-ca.p12")
      val clientSSLContext = SpecifiedSSLTrustStore(trustKeyStoreFile, None).sslContext
      clientSSLContext.getProtocol shouldBe "TLS"
      val clientTrustedCerts = trustedCertsFromSSLContext(clientSSLContext)

      clientTrustedCerts.head.getSubjectDN.getName shouldBe "CN=Topaz CA, O=Topaz, L=London, C=UK"

      // Test server keystore
      val serverKeyStoreFile = file(keyStoreDirectory, "server.p12")
      val serverPassword = readLines(file(keyStoreDirectory, "server-password")).head
      val serverSSLContext = SpecifiedSSLTrustStore(serverKeyStoreFile, Some(serverPassword)).sslContext
      val serverTrustedCerts = trustedCertsFromSSLContext(serverSSLContext)
      val serverTrustedDNs = serverTrustedCerts.map(_.getSubjectDN.getName)

      serverTrustedDNs should contain ("CN=Topaz CA, O=Topaz, L=London, C=UK")
      serverTrustedDNs should contain ("CN=test.topaz.technology, O=Topaz, L=London, C=UK")
    }
  }
}
