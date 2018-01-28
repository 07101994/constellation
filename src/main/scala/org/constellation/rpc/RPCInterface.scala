package org.constellation.rpc

import java.util.concurrent.TimeUnit

import akka.actor.ActorRef
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, PredefinedFromEntityUnmarshallers}
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.scalalogging.Logger
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import org.constellation.blockchain.{Block, GenesisBlock, Transaction}
import ChainInterface.{QueryAll, QueryLatest, ResponseBlock, ResponseBlockChain}
import akka.http.scaladsl.server.Route
import org.constellation.p2p.PeerToPeer._
import org.constellation.tx.AtomicTransaction.{DoubleEntryOriginBroadcast, TransactionInputData, TransactionRPCRequest}
import org.constellation.wallet.{KeyUtils, Wallet}
import org.json4s.native.Serialization
import org.json4s.{DefaultFormats, Formats, native}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object RPCInterface {

  var wallet : Option[Wallet] = None

}

trait RPCInterface extends Json4sSupport {

  val blockChainActor: ActorRef
  val logger: Logger

  implicit val serialization: Serialization.type = native.Serialization
  implicit val stringUnmarshallers: FromEntityUnmarshaller[String] = PredefinedFromEntityUnmarshallers.stringUnmarshaller

  implicit def json4sFormats: Formats = Wallet.walletFormats

  implicit val executionContext: ExecutionContext

  implicit val timeout: Timeout = Timeout(5, TimeUnit.SECONDS)

  import RPCInterface.wallet

  val routes: Route =
    get {
      path("wallet") {
        complete(wallet.getOrElse(Wallet.emptyWallet))
      } ~
        path("createWallet") {
          parameter('numPairs.as[Int].?) { numKeyPairsOpt =>
            val numKeyPairs = numKeyPairsOpt.getOrElse(1)
            wallet = Some(Wallet.create(numKeyPairs))
            complete(s"" +
              s"Created wallet by generating new keys with $numKeyPairs total key pairs using " +
              s"${KeyUtils.ECDSAKeyPairInstance} instance with ${KeyUtils.SECP256k1ParamSpec} spec " +
              s"and ${KeyUtils.SpongyCastleProviderCode} provider. " +
              s"Wallet is loaded in memory now but not saved anywhere.")
          }
        } ~
        path("saveWallet") {
          parameter('path.?) { savePathOpt =>
            val response = wallet match {
              case Some(w) =>
                Try{Wallet.save(w, savePathOpt)}.toOption.map{ _ =>
                  "Saved wallet"
                }.getOrElse("Failed to save wallet")
              case None =>
                "Cannot save wallet, none loaded in memory"
            }
            complete(response)
          }
        } ~
        path("blocks") {
          val chain: Future[Seq[Block]] = (blockChainActor ? QueryAll).map {
            //This is a bit of a hack, since JSON4S doesn't serialize case objects well
            case ResponseBlockChain(blockChain) => blockChain.blocks.slice(0, blockChain.blocks.length -1) :+ GenesisBlock.copy()
          }
          complete(chain)
        }~
        path("peers") {
          complete( (blockChainActor ? GetPeers).mapTo[Peers] )
        }~
        path("id") {
          complete( (blockChainActor ? GetId).mapTo[Id] )
        }~
        path("latestBlock") {
          complete( (blockChainActor ? QueryLatest).map {
            case ResponseBlock(GenesisBlock) => GenesisBlock.copy()
            case ResponseBlock(block) => block
          })
        } ~
        path("healthCheck") {
          complete("Constellation BlockChain RPC API healthy")
        } ~
        path("") {
          complete {
            "Constellation"
          }
        }
    }~
      post {
        path("mineBlock") {
          entity(as[Transaction]) { data =>
            logger.info(s"Got request to add new block $data")
            complete((blockChainActor ? data).mapTo[ResponseBlock].map {
              case ResponseBlock(block) => block
            })
          }
        }~
          path("addPeer") {
            entity(as[String]) { peerAddress =>
              logger.info(s"Got request to add new peer $peerAddress")
              blockChainActor ! AddPeer(peerAddress)
              complete(s"Added peer $peerAddress")
            }
          } ~
          path("transaction") {
            entity(as[TransactionRPCRequest]) { tx =>
              complete {
                wallet.map { w =>
                  w.keyPairs.collectFirst {
                    case kp if KeyUtils.publicKeyToAddress(kp.getPublic) == tx.sourceAddress => kp
                  }.map { kp =>
                    val txInput = TransactionInputData(kp.getPublic, tx.destinationAddress, tx.quantity)
                    val dblEntry = DoubleEntryOriginBroadcast(txInput.toSigned(kp.getPrivate))
                    blockChainActor ! dblEntry
                    "Signed and broadcasted transaction to network via BlockChainActor"
                  }.getOrElse("Address not found in wallet")
                }.getOrElse("No wallet loaded")
              }
            }
          }
      }
}
