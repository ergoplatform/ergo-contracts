package org.ergoplatform.contracts.tests.dex

import org.ergoplatform.contracts.{
  DexBuyerContractParameters,
  DexLimitOrderContracts,
  DexSellerContractParameters
}
import org.scalacheck.Gen
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.serialization.generators.ObjectGenerators

import scala.language.implicitConversions
import sigmastate.helpers.ErgoLikeTestInterpreter
import sigmastate.helpers.ContextEnrichingTestProvingInterpreter
import org.ergoplatform.ErgoLikeTransaction
import org.ergoplatform.ErgoLikeContext
import org.ergoplatform.ErgoBox
import sigmastate.helpers.ErgoLikeContextTesting
import sigmastate.AvlTreeData
import special.sigma.SigmaProp
import sigmastate.eval.CSigmaProp
import scorex.crypto.hash.Digest32
import sigmastate.interpreter.ProverResult
import sigmastate.Values.ByteArrayConstant
import org.ergoplatform.DataInput
import org.ergoplatform.Input
import sigmastate.Values.LongConstant
import org.ergoplatform.ErgoAddress
import org.ergoplatform.P2PKAddress

class DexSellOrderSpreadTest
  extends SigmaTestingCommons
  with ObjectGenerators
  with DexTestingCommons {

  case class BuyOrder(tokenPrice: Long, tokenAmount: Long, creationHeight: Int)

  private def checkSpread(
    sellerTokenPrice: Long,
    sellerTokenAmount: Long,
    sellOrderCreationHeight: Int,
    buyOrderParams: IndexedSeq[BuyOrder],
    receivedSpread: Long
  ) = {
    val verifier = new ErgoLikeTestInterpreter
    val buyerPk =
      (new ContextEnrichingTestProvingInterpreter).dlogSecrets.head.publicImage
    val sellerPk =
      (new ContextEnrichingTestProvingInterpreter).dlogSecrets.head.publicImage

    val tokenId        = tokenIdGen.sample.get
    val dexFeePerToken = 1000000L

    val sellOrderContractParams =
      DexSellerContractParameters(sellerPk, tokenId, sellerTokenPrice, dexFeePerToken)

    val sellOrderContract =
      DexLimitOrderContracts.sellerContractInstance(sellOrderContractParams)

    val sellOrderBox = ErgoBox(
      value            = sellerTokenAmount * dexFeePerToken,
      ergoTree         = sellOrderContract.ergoTree,
      creationHeight   = sellOrderCreationHeight,
      additionalTokens = Seq((Digest32 @@ tokenId, sellerTokenAmount)),
      additionalRegisters = Map(
        ErgoBox.R4 -> ByteArrayConstant(tokenId),
        ErgoBox.R5 -> LongConstant(sellerTokenPrice)
      )
    )

    val currentHeight =
      (sellOrderCreationHeight +: buyOrderParams.map(_.creationHeight)).sorted.last + 1

    val returnBox = ErgoBox(
      value               = sellerTokenPrice * sellerTokenAmount + receivedSpread,
      ergoTree            = sellerPk,
      creationHeight      = currentHeight,
      additionalRegisters = Map(ErgoBox.R4 -> ByteArrayConstant(sellOrderBox.id))
    )

    val buyOrders = buyOrderParams.map { p =>
      ErgoBox(
        value = p.tokenAmount * (p.tokenPrice + dexFeePerToken),
        ergoTree = DexLimitOrderContracts
          .buyerContractInstance(
            DexBuyerContractParameters(buyerPk, tokenId, p.tokenPrice, dexFeePerToken)
          )
          .ergoTree,
        creationHeight = p.creationHeight,
        additionalRegisters = Map(
          ErgoBox.R4 -> ByteArrayConstant(tokenId),
          ErgoBox.R5 -> LongConstant(p.tokenPrice),
          ErgoBox.R6 -> LongConstant(dexFeePerToken)
        )
      )
    }

    val boxesToSpend = buyOrders ++ IndexedSeq(sellOrderBox)
    val tx = new ErgoLikeTransaction(
      boxesToSpend.map(b => Input(b.id, ProverResult.empty)),
      IndexedSeq(),
      IndexedSeq(returnBox)
    )

    val context = ErgoLikeContextTesting(
      currentHeight       = currentHeight,
      lastBlockUtxoRoot   = AvlTreeData.dummy,
      minerPubkey         = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend        = boxesToSpend,
      spendingTransaction = tx,
      self                = sellOrderBox
    )

    verifier
      .verify(sellOrderBox.ergoTree, context, ProverResult.empty, fakeMessage)
      .get
      ._1
  }

  property("same height, same price") {
    val sellerTokenPrice        = 20000000L
    val sellerTokenAmount       = 100L
    val sellOrderCreationHeight = 100

    checkSpread(
      sellerTokenPrice,
      sellerTokenAmount,
      sellOrderCreationHeight,
      IndexedSeq(
        BuyOrder(sellerTokenPrice, sellerTokenAmount / 2, sellOrderCreationHeight),
        BuyOrder(sellerTokenPrice, sellerTokenAmount / 2, sellOrderCreationHeight)
      ),
      receivedSpread = 0L
    ) shouldBe true
  }

  property("older buy orders, same price") {
    val sellerTokenPrice        = 20000000L
    val sellerTokenAmount       = 100L
    val sellOrderCreationHeight = 100

    checkSpread(
      sellerTokenPrice,
      sellerTokenAmount,
      sellOrderCreationHeight,
      IndexedSeq(
        BuyOrder(sellerTokenPrice, sellerTokenAmount / 2, sellOrderCreationHeight + 1),
        BuyOrder(sellerTokenPrice, sellerTokenAmount / 2, sellOrderCreationHeight + 1)
      ),
      receivedSpread = 0L
    ) shouldBe true
  }

  property("older buy orders, different price") {
    val sellerTokenPrice        = 20000000L
    val sellerTokenAmount       = 100L
    val sellOrderCreationHeight = 100

    checkSpread(
      sellerTokenPrice,
      sellerTokenAmount,
      sellOrderCreationHeight,
      IndexedSeq(
        BuyOrder(
          sellerTokenPrice + 2,
          sellerTokenAmount / 2,
          sellOrderCreationHeight + 1
        ),
        BuyOrder(
          sellerTokenPrice + 1,
          sellerTokenAmount / 2,
          sellOrderCreationHeight + 1
        )
      ),
      receivedSpread = (sellerTokenAmount / 2) * 1 + (sellerTokenAmount / 2) * 2
    ) shouldBe true
  }

  property("mixed buy orders height") {
    val sellerTokenPrice        = 20000000L
    val sellerTokenAmount       = 99L
    val sellOrderCreationHeight = 100

    val buyOrderWithSpread2 = BuyOrder(
      sellerTokenPrice + 2,
      sellerTokenAmount / 3,
      sellOrderCreationHeight + 1
    )

    val buyOrderWithSpread1 = BuyOrder(
      sellerTokenPrice + 1,
      sellerTokenAmount / 3,
      sellOrderCreationHeight + 1
    )

    // Although the price is the highest, spread is not ours (height < sell order's height)
    val buyOrderWithSpread0 = BuyOrder(
      sellerTokenPrice + 5,
      sellerTokenAmount / 3,
      sellOrderCreationHeight - 1
    )

    // fine (box with lost spread is last)
    checkSpread(
      sellerTokenPrice,
      sellerTokenAmount,
      sellOrderCreationHeight,
      IndexedSeq(
        buyOrderWithSpread2,
        buyOrderWithSpread1,
        buyOrderWithSpread0
      ),
      receivedSpread = (sellerTokenAmount / 3) * 2 + (sellerTokenAmount / 3) * 1
    ) shouldBe true

    // fail (first box with lost spread)
    checkSpread(
      sellerTokenPrice,
      sellerTokenAmount,
      sellOrderCreationHeight,
      IndexedSeq(
        buyOrderWithSpread0,
        buyOrderWithSpread2,
        buyOrderWithSpread1
      ),
      receivedSpread = (sellerTokenAmount / 3) * 2 + (sellerTokenAmount / 3) * 1
    ) shouldBe false

    // also fail (box with lost spread before the box with won spread)
    checkSpread(
      sellerTokenPrice,
      sellerTokenAmount,
      sellOrderCreationHeight,
      IndexedSeq(
        buyOrderWithSpread2,
        buyOrderWithSpread0,
        buyOrderWithSpread1
      ),
      receivedSpread = (sellerTokenAmount / 3) * 2 + (sellerTokenAmount / 3) * 1
    ) shouldBe false
  }

}
