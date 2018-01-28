package org.constellation.wallet

import java.io.File
import java.security.KeyPair

import org.constellation.wallet.KeyUtils.{KeyPairSerializer, PrivateKeySerializer, PublicKeySerializer}
import org.json4s.native.Serialization
import org.json4s.{DefaultFormats, Formats}

import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

case class Wallet(keyPairs: Seq[KeyPair])

object Wallet {

  implicit val walletFormats: Formats = DefaultFormats +
    new PublicKeySerializer + new PrivateKeySerializer + new KeyPairSerializer

  def create(numKeyPairs: Int) = Wallet(Seq.fill(numKeyPairs)(KeyUtils.makeKeyPair()))

  private def homeDir: String = System.getProperty("user.home")

  private val dataDir = new File(homeDir, ".constellation")
  Try{dataDir.mkdir()}
  val defaultStoragePath = new File(dataDir, "wallet.json")

  def loadDefaultOrPath(pathOpt: Option[String]) = Try{Serialization.read[Wallet]({
    Source.fromFile(pathOpt.map{new File(_)}.getOrElse(defaultStoragePath))
  }.getLines().mkString)}

  def save(wallet: Wallet, pathOpt: Option[String]): Unit = {
    loadDefaultOrPath(pathOpt) match {
      case Success(w) =>
        // Found an existing wallet in default location
        // Need to back it up just as a precaution
        val backup = new File(dataDir, s"backup_wallet_${System.currentTimeMillis()}.json")
        scala.tools.nsc.io.File(backup).writeAll(Serialization.write(w))
      case Failure(e) =>
        scala.tools.nsc.io.File(defaultStoragePath).writeAll(Serialization.write(wallet))
    }
  }

}