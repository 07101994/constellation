package org.constellation.blockchain

import org.constellation.actor.Receiver
import org.constellation.rpc.ChainInterface.ResponseBlock
import org.constellation.blockchain.Consensus.MineBlock
import org.constellation.p2p.PeerToPeer
import org.constellation.rpc.ChainInterface
import org.constellation.tx.AtomicTransaction.{DoubleEntryOriginBroadcast, DoubleEntrySignedTransaction}

object Consensus {
  case class MineBlock( data: String )
}


trait Consensus {
  this: ChainInterface with PeerToPeer with Receiver =>



  receiver {
    case transaction: Transaction =>
      blockChain = blockChain.addBlock(transaction.message)
      val peerMessage = ResponseBlock(blockChain.latestBlock)
      broadcast(peerMessage)
      sender() ! peerMessage
    case tx: DoubleEntrySignedTransaction =>
      logger.info(s"Received DEST: $tx")
      if (tx.verified) {

      }

      // This came from an RPC request by the initiator. It's not from the network
    case tx: DoubleEntryOriginBroadcast =>
      // Eventually, we need to broadcast the messages separately to optimize throughput
      // Messages will not reach the entire network in one round of transactions
      // As peers will only be able to talk to a small neighborhood of other peers.
      // For now this is totally fine
      broadcast(tx.tx2)
      logger.info(s"Received double entry origin broadcast tx: $tx")

  }
}
