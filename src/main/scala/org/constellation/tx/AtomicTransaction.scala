package org.constellation.tx

import java.nio.ByteBuffer
import java.security.{PrivateKey, PublicKey}

import org.constellation.wallet.KeyUtils
import org.constellation.wallet.KeyUtils.{base64, bytesToPublicKey, fromBase64}
import org.json4s.native.Serialization

object AtomicTransaction {

  case class TransactionRPCRequest(
                                    sourceAddress: String,
                                    destinationAddress: String,
                                    quantity: Long
                                  )

  def bytesToLong(bytes: Array[Byte]): Long = {
    val buffer = ByteBuffer.allocate(8)
    buffer.put(bytes)
    buffer.flip();//need flip
    buffer.getLong()
  }

  def longToBytes(l: Long): Array[Byte] = {
    val buffer = ByteBuffer.allocate(8)
    buffer.putLong(0, l)
    buffer.array()
  }

  case class DoubleEntryOriginBroadcast(tx2: DoubleEntrySignedTransaction)

  case class DoubleEntrySignedTransaction(
                                           debit: SignedTransaction,
                                           credit: SignedTransaction
                                         ) {
    def verified: Boolean = debit.verified && credit.verified
  }

  case class SignedTransaction(
                                transactionInputData: TransactionInputData,
                                signedOutput: Array[Byte]
                              ) {
    def verified: Boolean = {
      KeyUtils.verifySignature(transactionInputData.bytes, signedOutput)
    }
  }

  // TODO: Change these to use json4s custom serializers.
  // Working on this now in the wallet class
  // Example: https://nmatpt.com/blog/2017/01/29/json4s-custom-serializer/
  // I am not sure whether or not we'll need to use this case class at the
  // encoded stage at some point. Need to figure this out.
  case class TransactionInputData(
                                   sourcePubKey: PublicKey,
                                   destinationAddress: String,
                                   quantity: Long
                                 ) {
    def encode = EncodedTransaction(
      base64(sourcePubKey.getEncoded),
      destinationAddress,
      base64(longToBytes(quantity))
    )

    def bytes: Array[Byte] = encode.rendered.getBytes

    def sourceAddress: String = KeyUtils.publicKeyToAddress(sourcePubKey)

    def toSigned(privateKey: PrivateKey): DoubleEntrySignedTransaction = {
      val debit = SignedTransaction(this, KeyUtils.signData(bytes)(privateKey))
      val creditInput = this.copy(destinationAddress = sourceAddress, quantity = -1*quantity.toLong)
      val credit = SignedTransaction(creditInput,
        KeyUtils.signData(creditInput.bytes)(privateKey)
      )
      DoubleEntrySignedTransaction(debit, credit)
    }

  }

  case class EncodedTransaction(
                                 sourceAddress: String,
                                 destinationAddress: String,
                                 quantity: String
                               ) {
    def decode = TransactionInputData(
      bytesToPublicKey(fromBase64(sourceAddress)),
      destinationAddress,
      bytesToLong(fromBase64(quantity))
    )
    def ordered = Array(sourceAddress, destinationAddress, quantity)
    def rendered: String = {
      import org.json4s._
      implicit val f: DefaultFormats.type = DefaultFormats
      Serialization.write(ordered)
    }
  }

  def txFromArray(array: Array[String]): EncodedTransaction = {
    val Array(sourceAddress, destinationAddress, quantity) = array
    EncodedTransaction(sourceAddress, destinationAddress, quantity)
  }

  def txFromString(rendered: String): EncodedTransaction = {
    import org.json4s._
    implicit val f: DefaultFormats.type = DefaultFormats
    val array = Serialization.read[Array[String]](rendered)
    txFromArray(array)
  }

}
