package org.constellation.rpc

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import org.json4s.Formats
import org.json4s.native.Serialization

import scala.concurrent.{ExecutionContextExecutor, Future}


// TODO : Implement all methods from RPCInterface here for a client SDK

class RPCClient(host: String = "127.0.0.1", port: Int)(
  implicit val system: ActorSystem,
  implicit val materialize: ActorMaterializer,
  implicit val executionContext: ExecutionContextExecutor,
) {

  val baseURI = s"http://$host:$port"
  def base(suffix: String) = Uri(s"$baseURI/$suffix")
  def query(suffix: String, queryParams: Map[String,String] = Map()): Future[HttpResponse] = {
    Http().singleRequest(
      HttpRequest(uri = base(suffix).withQuery(Query(queryParams)))
    )
  }

  def post[T <: AnyRef](suffix: String, t: T)(implicit f : Formats): Future[HttpResponse] = {
    val ser = Serialization.write(t)
    Http().singleRequest(
      HttpRequest(uri = base(suffix), method = HttpMethods.POST, entity = HttpEntity(
        ContentTypes.`application/json`, ser)
      )
    )
  }

  def createWallet(numKeyPairs: Int = 1): Future[HttpResponse] =
    query("createWallet", Map("numPairs" -> numKeyPairs.toString))

  def wallet(): Future[HttpResponse] = query("wallet")

}