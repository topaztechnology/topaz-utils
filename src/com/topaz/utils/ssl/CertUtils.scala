package com.topaz.utils.ssl

import java.math.BigInteger
import java.security._
import java.util.{Arrays => JArrays, Vector => JVector}

import sun.security.util.ObjectIdentifier
import sun.security.x509._


object CertUtils {

  sealed trait CertType {
    def distinguishedName: String
    def validity: CertificateValidity
  }

  case class CACert(
    distinguishedName: String,
    validity: CertificateValidity
  ) extends CertType

  case class ServerCert(
    distinguishedName: String,
    dnsName: String,
    validity: CertificateValidity,
    issuerDistinguishedName: String,
    caPublicKey: PublicKey
  ) extends CertType

  def generateRSAKeyPair(keySize: Int): KeyPair = {
    val keyGen = KeyPairGenerator.getInstance("RSA")
    keyGen.initialize(keySize)
    keyGen.generateKeyPair()
  }

  def createCertInfo(certType: CertType, keyPair: KeyPair, signatureAlgo: String): X509CertInfo = {
    val serialNo = new BigInteger(64, new SecureRandom())
    val subjectName = new X500Name(certType.distinguishedName)
    val algoOid = AlgorithmId.get(signatureAlgo)

    val info = new X509CertInfo()
    info.set(X509CertInfo.VALIDITY, certType.validity)
    info.set(X509CertInfo.SERIAL_NUMBER, new CertificateSerialNumber(serialNo))
    info.set(X509CertInfo.SUBJECT, subjectName)
    val issuerName = certType match {
      case _: CACert => subjectName
      case ServerCert(_, _, _, issuerDN, _) => new X500Name(issuerDN)
    }
    info.set(X509CertInfo.ISSUER, issuerName)
    info.set(X509CertInfo.KEY, new CertificateX509Key(keyPair.getPublic))
    info.set(X509CertInfo.VERSION, new CertificateVersion(CertificateVersion.V3))
    info.set(X509CertInfo.ALGORITHM_ID, new CertificateAlgorithmId(algoOid))

    val extensions = certType match {
      case _: CACert =>
        val keyUsageExtension = new KeyUsageExtension()
        keyUsageExtension.set(KeyUsageExtension.KEY_CERTSIGN, true)

        val basicConstraintsExtension = new BasicConstraintsExtension(true, -1) // Set IS_CA true

        val extensions = new CertificateExtensions()
        extensions.set(KeyUsageExtension.IDENT, keyUsageExtension)
        extensions.set(BasicConstraintsExtension.IDENT, basicConstraintsExtension)
        extensions

      case ServerCert(_, dnsName, _, _, caPublicKey) =>
        val keyUsageExtension = new KeyUsageExtension()
        keyUsageExtension.set(KeyUsageExtension.DIGITAL_SIGNATURE, true)
        keyUsageExtension.set(KeyUsageExtension.KEY_ENCIPHERMENT, true)

        val extendedKeyUsageExtension = new ExtendedKeyUsageExtension(false,
          new JVector[ObjectIdentifier](JArrays.asList(new ObjectIdentifier("1.3.6.1.5.5.7.3.1"))) // serverAuth
        )

        val subjectAlternativeNameExtension = new SubjectAlternativeNameExtension(
          new GeneralNames().add(new GeneralName(new DNSName(dnsName)))
        )

        val authorityKeyIdentifierExtension = new AuthorityKeyIdentifierExtension(
          new KeyIdentifier(caPublicKey), null, null)

        val extensions = new CertificateExtensions()
        extensions.set(AuthorityKeyIdentifierExtension.IDENT, authorityKeyIdentifierExtension)
        extensions.set(KeyUsageExtension.IDENT, keyUsageExtension)
        extensions.set(ExtendedKeyUsageExtension.IDENT, extendedKeyUsageExtension)
        extensions.set(SubjectAlternativeNameExtension.IDENT, subjectAlternativeNameExtension)
        extensions
    }

    val subjectKeyIdentifierExtension = new SubjectKeyIdentifierExtension(new KeyIdentifier(keyPair.getPublic).getIdentifier)
    extensions.set(SubjectKeyIdentifierExtension.IDENT, subjectKeyIdentifierExtension)
    info.set(X509CertInfo.EXTENSIONS, extensions)

    info
  }

  def signCert(certInfo: X509CertInfo, privateKey: PrivateKey, signatureAlgo: String): X509CertImpl = {
    val testCert = new X509CertImpl(certInfo)
    testCert.sign(privateKey, signatureAlgo)

    val usedAlgo = testCert.get(X509CertImpl.SIG_ALG)
    certInfo.set(s"${CertificateAlgorithmId.NAME}.${CertificateAlgorithmId.ALGORITHM}", usedAlgo)
    val cert = new X509CertImpl(certInfo)
    cert.sign(privateKey, signatureAlgo)

    cert
  }
}
