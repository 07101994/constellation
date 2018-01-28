import java.security.KeyPair

import org.constellation.wallet.KeyUtils

import scala.concurrent.{Await, Future}

package object cst {

  implicit class EasyFutureBlock[T](f: Future[T]) {
    def get(t: Int = 5): T = {
      import scala.concurrent.duration._
      Await.result(f, t.seconds)
    }
  }

  implicit class KeyPairHelpers(kp: KeyPair) {
    def address: String = KeyUtils.publicKeyToAddress(kp.getPublic)
  }

}
