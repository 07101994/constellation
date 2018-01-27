package org.constellation.wallet

import java.io.File
import java.security.KeyPair

import org.constellation.wallet.KeyUtils.{KeyPairSerializer, PrivateKeySerializer, PublicKeySerializer}
import org.json4s.native.Serialization
import org.json4s.{DefaultFormats, Formats}

import scala.io.{BufferedSource, Source}
import scala.util.Try

case class Wallet(keyPairs: Seq[KeyPair])

object Wallet {

  implicit val walletFormats: Formats = DefaultFormats +
    new PublicKeySerializer + new PrivateKeySerializer + new KeyPairSerializer

  def create(numKeyPairs: Int) = Wallet(Seq.fill(numKeyPairs)(KeyUtils.makeKeyPair()))

  private def homeDir: String = System.getProperty("user.home")

  val defaultStoragePath = new File(new File(homeDir, ".constellation"), "wallet.json")

  def loadDefault() = Try{Serialization.read[Wallet]({
    Source.fromFile(defaultStoragePath)
  }.getLines().mkString)}

}