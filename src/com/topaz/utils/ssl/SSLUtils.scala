package com.topaz.utils.ssl

import java.io.{File, FileInputStream, FileOutputStream}
import java.security.cert.Certificate
import java.security.{KeyPair, KeyStore, PrivateKey}
import java.util.Date

import com.topaz.utils.Log
import com.topaz.utils.StringUtils.SecureRandomString
import com.typesafe.config.ConfigFactory
import sun.security.x509.CertificateValidity


object SSLUtils extends Log {
  val TOPAZ_CA_ALIAS = "topaz-ca"
  val TOPAZ_CA_FILENAME = "topaz-ca.p12"
  val SERVER_ALIAS = "server"
  val NULL_PASSWORD = "nopasswordneeded".toCharArray

  def initialise(sslConfig: ServerSSLConfig): Unit = {
    import CertUtils._
    import ServerSSLConfig._

    sslConfig match {
      case conf: Generated =>
        val keyStoresDirectory = conf.storeDirectory
        val trustKeyStoreFile = conf.caFile
        val serverKeyStoreFile = conf.keyStoreFile
        val serverPasswordFile = conf.passwordFile

        if (!trustKeyStoreFile.exists() || !serverKeyStoreFile.exists() || !serverPasswordFile.exists()) {
          log.warn("One or more SSL related files do not exist, will regenerate as required")
          keyStoresDirectory.mkdirs()

          val generatedConfig = conf.generatedConf

          val signatureAlgo = generatedConfig.getString("signature-algorithm")
          val dnSuffix = generatedConfig.getString("distinguished-name-suffix")

          def distinguishedName(name: String) = s"CN=$name, $dnSuffix"

          def validity(days: Int): CertificateValidity = {
            val from = new Date() // Valid from now
            val to = new Date(from.getTime + days * 86400000l)
            new CertificateValidity(from, to)
          }

          val caConfig = generatedConfig.getConfig("certificate-authority")
          val caKeyStoreFile = new File(keyStoresDirectory, caConfig.getString("keystore-filename"))
          val caPasswordFile = new File(keyStoresDirectory, caConfig.getString("password-filename"))
          val caDN = distinguishedName(caConfig.getString("name"))

          def caCertAndKeyPair(): (Certificate, KeyPair) = {
            val keyStore = KeyStore.getInstance("PKCS12")
            val caPassword = readLines(caPasswordFile).head.toCharArray
            keyStore.load(new FileInputStream(caKeyStoreFile), caPassword)
            val cert = keyStore.getCertificate(TOPAZ_CA_ALIAS)
            val privateKey = keyStore.getKey(TOPAZ_CA_ALIAS, caPassword).asInstanceOf[PrivateKey]
            val keyPair = new KeyPair(cert.getPublicKey, privateKey)
            (cert, keyPair)
          }

          val (caCert, caKeyPair, isNewCA) = if (!trustKeyStoreFile.exists()) {
            // We don't have a trust keystore containing the CA cert which the client will need to trust the server.

            // First check if the CA keystore with keys exists, and if not regenerate
            val (caCert, caKeyPair, isNewCA) = if (!caKeyStoreFile.exists() || !caPasswordFile.exists()) {
              log.info("CA keystore and / or password files do not exist, will regenerate")
              val caCertType = CACert(caDN, validity(caConfig.getInt("validity")))

              log.debug(s"Generating CA cert with DN: ${caCertType.distinguishedName}")
              val newCAKeyPair = generateRSAKeyPair(caConfig.getInt("key-size"))
              val caCertInfo = createCertInfo(caCertType, newCAKeyPair, signatureAlgo)
              val newCACert = signCert(caCertInfo, newCAKeyPair.getPrivate, signatureAlgo)

              log.debug(s"Writing CA keystore password to file: ${caPasswordFile.getAbsolutePath}")
              val caPassword = SecureRandomString.nextString(24)
              writeToFile(caPasswordFile, caPassword, append = false)

              log.debug(s"Writing CA cert with keys to keystore: ${caKeyStoreFile.getAbsolutePath}")
              val keyStore = KeyStore.getInstance("PKCS12")
              keyStore.load(null, null)
              keyStore.setKeyEntry(TOPAZ_CA_ALIAS, newCAKeyPair.getPrivate, caPassword.toCharArray, Array(newCACert))
              keyStore.store(new FileOutputStream(caKeyStoreFile), caPassword.toCharArray)

              (newCACert, newCAKeyPair, true)
            } else {
              // CA keystore does exist, so extract its contents
              val (existingCACert, existingKeyPair) = caCertAndKeyPair()

              (existingCACert, existingKeyPair, false)
            }

            log.debug(s"Writing CA cert to trust keystore: ${trustKeyStoreFile.getAbsolutePath}")
            val trustKeyStore = KeyStore.getInstance("PKCS12")
            trustKeyStore.load(null, null)
            trustKeyStore.setCertificateEntry(TOPAZ_CA_ALIAS, caCert)
            trustKeyStore.store(new FileOutputStream(trustKeyStoreFile), NULL_PASSWORD)

            (caCert, caKeyPair, isNewCA)
          } else {
            // CA keystore and trust keystore exist, but we need the CA cert and keys for the server cert
            val (existingCACert, existingKeyPair) = caCertAndKeyPair()

            (existingCACert, existingKeyPair, false)
          }

          if (!serverKeyStoreFile.exists() || !serverPasswordFile.exists() || isNewCA) {
            if (isNewCA)
              log.info("We have a new CA, so we need to regenerate the server keystore so it's trusted")
            else
              log.info("Server keystore and / or password files do not exist, will regenerate")

            val serverConfig = generatedConfig.getConfig("server")
            val serverCertType = ServerCert(
              distinguishedName(serverConfig.getString("name")),
              serverConfig.getString("name"),
              validity(serverConfig.getInt("validity")),
              caDN,
              caKeyPair.getPublic
            )

            log.debug(s"Generating server cert with DN: ${serverCertType.distinguishedName}")
            val serverKeyPair = generateRSAKeyPair(serverConfig.getInt("key-size"))
            val serverCertInfo = createCertInfo(serverCertType, serverKeyPair, signatureAlgo)
            val serverCert = signCert(serverCertInfo, caKeyPair.getPrivate, signatureAlgo)

            log.debug(s"Writing server keystore password to file: ${serverPasswordFile.getAbsolutePath}")
            val serverPassword = SecureRandomString.nextString(24)
            writeToFile(serverPasswordFile, serverPassword, append = false)

            log.debug(s"Writing server cert with keys to keystore: ${serverKeyStoreFile.getAbsolutePath}")
            val serverKeyStore = KeyStore.getInstance("PKCS12")
            serverKeyStore.load(null, null)
            serverKeyStore.setKeyEntry(SERVER_ALIAS, serverKeyPair.getPrivate, serverPassword.toCharArray, Array(serverCert))
            serverKeyStore.setCertificateEntry(TOPAZ_CA_ALIAS, caCert)
            serverKeyStore.store(new FileOutputStream(serverKeyStoreFile), serverPassword.toCharArray)
          }
        }

      case _ =>
    }
  }

  // Handy for testing certificate generation.
  // KeyStore Explorer is a useful tool for examining key stores: https://keystore-explorer.org/downloads.html
  def main(args: Array[String]): Unit = {
    val sslConfig = ConfigFactory.parseString(
      """
        |status = generated
        |directory = "."
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

    initialise(ServerSSLConfig(sslConfig))
  }
}
