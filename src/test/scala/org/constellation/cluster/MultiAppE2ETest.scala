package org.constellation.cluster

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.testkit.RouteTest
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.Timeout
import org.constellation.app.AppNode
import org.constellation.p2p.PeerToPeer.{GetPeers, Peers}
import org.constellation.rpc.RPCClient
import org.scalatest.{BeforeAndAfterAll, FlatSpec}
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, PredefinedFromEntityUnmarshallers, Unmarshal}

import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}
import cst._
import org.constellation.tx.AtomicTransaction.TransactionRPCRequest
import org.constellation.wallet.Wallet
import org.json4s.native.Serialization
import org.json4s.{Formats, native}

class MultiAppE2ETest extends FlatSpec with BeforeAndAfterAll {

  implicit val system: ActorSystem = ActorSystem("BlockChain")
  implicit val materialize: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  "Cluster BlockChain" should "create a BlockChain from scratch" in {

    val seedHostNode = AppNode.apply()

    val seedRPC = new RPCClient(port=seedHostNode.httpPort)

    // Need one keypair for generating the 'genesis transaction'
    // Another to secretly receive it.
    val responseFuture = seedRPC.createWallet(2)

    implicit val serialization: Serialization.type = native.Serialization
    implicit val stringUnmarshallers: FromEntityUnmarshaller[String] = PredefinedFromEntityUnmarshallers.stringUnmarshaller

    implicit def json4sFormats: Formats = Wallet.walletFormats

    println(responseFuture.get())

    def uread[T <: AnyRef](httpResponse: HttpResponse)(implicit m : Manifest[T]) =
      Serialization.read[T](Unmarshal(httpResponse.entity).to[String].get())

    val u = uread[Wallet](seedRPC.wallet().get())

    val origin = u.keyPairs.head
    val dest = u.keyPairs(1)


    implicit val timeout: Timeout = seedHostNode.timeout

    val seedHostPath = seedHostNode.blockChainActor.path.toSerializationFormat



    val nodes = (1 to 3).map { _ =>
      AppNode.apply(seedHostPath)
    }

    nodes.foreach{ n =>
      import scala.concurrent.duration._
      val response = Await.result(n.blockChainActor ? GetPeers, 5.seconds).asInstanceOf[Peers]
      assert(response.peers.nonEmpty)
    }

    println(
      seedRPC.post("transaction", TransactionRPCRequest(origin.address, dest.address, 1e9.toLong)).get()
    )



    // Need to verify nodes are healthy

  }

  override def afterAll() {
    system.terminate()
  }

}
