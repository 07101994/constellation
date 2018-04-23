package org.constellation.util

import java.security.{KeyPair, PrivateKey, PublicKey}

import constellation._

object POW extends POWExt

trait POWExt {

  def proofOfWork(input: String, difficulty: Option[Int]): String = {
    var done = false
    var count = 0L
    while (!done) {
      count += 1
      if (verifyPOW(input, count.toString, difficulty)) {
        done = true
      }
    }
    count.toString
  }

  def hashNonce(input: String, nonce: String): String = {
    val strNonced = input + nonce
    strNonced.sha256.sha256
  }

  def verifyPOW(input: String, nonce: String, difficulty: Option[Int]): Boolean = {
    val sha = hashNonce(input, nonce)
    difficulty.exists{d => sha.startsWith("0"*d)}
  }

}

trait ProductHash extends Product {

  def signInput: Array[Byte] = productSeq.json.getBytes()
  def signKeys(privateKeys: Seq[PrivateKey]): Seq[String] = privateKeys.map { pk => base64(signData(signInput)(pk)) }
  def powInput(signatures: Seq[String]): String = (productSeq ++ signatures).json
  def pow(signatures: Seq[String], difficulty: Int): String = POW.proofOfWork(powInput(signatures), Some(difficulty))
  def productSeq: Seq[Any] = this.productIterator.toArray.toSeq

}


// TODO: Extend to monad, i.e. SignedData extends SignableData vs UnsignedData
case class Signed[T <: ProductHash](
                                          data: T,
                                          startTime: Long,
                                          endTime: Long,
                                          nonce: String,
                                          difficulty: Int,
                                          publicKeys: Seq[PublicKey],
                                          signatures: Seq[String]
                                        ) extends ProductHash {
  def validSignatures: Boolean = signatures.zip(publicKeys).forall{ case (sig, pub) =>
    verifySignature(data.signInput, fromBase64(sig))(pub) && signatures.nonEmpty && publicKeys.nonEmpty
  }
  def validPOW: Boolean = POW.verifyPOW(data.powInput(signatures), nonce, Some(difficulty))
  def valid: Boolean = validSignatures && validPOW && startTime > minimumTime &&
    endTime > minimumTime

}

trait POWSignHelp {

  implicit class LinearHashHelpers[T <: ProductHash](t: T) {
    def sign2(keyPairs: Seq[KeyPair], difficulty: Int = 1): Signed[T] =
      sign[T](t, keyPairs, difficulty)
  }

  def sign[T <: ProductHash](
                                     t: T,
                                     keyPairs: Seq[KeyPair],
                                     difficulty: Int = 0
                                   ): Signed[T] = {
    val startTime = System.currentTimeMillis()
    val signatures = t.signKeys(keyPairs.map{_.getPrivate})
    val nonce = t.pow(signatures, difficulty)
    val endTime = System.currentTimeMillis()
    Signed(
      t, startTime, endTime, nonce, difficulty, keyPairs.map{_.getPublic}, signatures
    )
  }

}