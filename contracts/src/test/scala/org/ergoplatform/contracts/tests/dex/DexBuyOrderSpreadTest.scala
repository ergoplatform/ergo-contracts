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

class DexBuyOrderSpreadTest
  extends SigmaTestingCommons
  with ObjectGenerators
  with DexTestingCommons {

  case class SellOrder(tokenPrice: Long, tokenAmount: Long, creationHeight: Int)

  private def checkSpread(
    buyerTokenPrice: Long,
    buyerTokenAmount: Long,
    buyOrderCreationHeight: Int,
    sellOrderParams: IndexedSeq[SellOrder],
    receivedSpread: Long
  ) = {
    val verifier = new ErgoLikeTestInterpreter
    val buyerPk =
      (new ContextEnrichingTestProvingInterpreter).dlogSecrets.head.publicImage
    val sellerPk =
      (new ContextEnrichingTestProvingInterpreter).dlogSecrets.head.publicImage

    val tokenId        = tokenIdGen.sample.get
    val dexFeePerToken = 1000000L

    val buyOrderContractParams =
      DexBuyerContractParameters(buyerPk, tokenId, buyerTokenPrice, dexFeePerToken)

    val buyOrderContract =
      DexLimitOrderContracts.buyerContractInstance(buyOrderContractParams)

    val buyOrderBox = ErgoBox(
      value          = buyerTokenAmount * (dexFeePerToken + buyerTokenPrice),
      ergoTree       = buyOrderContract.ergoTree,
      creationHeight = buyOrderCreationHeight,
      additionalRegisters = Map(
        ErgoBox.R4 -> ByteArrayConstant(tokenId),
        ErgoBox.R5 -> LongConstant(buyerTokenPrice)
      )
    )

    val currentHeight =
      (buyOrderCreationHeight +: sellOrderParams.map(_.creationHeight)).sorted.last + 1

    val returnBox = ErgoBox(
      value               = receivedSpread,
      ergoTree            = buyerPk,
      creationHeight      = currentHeight,
      additionalTokens    = Seq((Digest32 @@ tokenId, buyerTokenAmount)),
      additionalRegisters = Map(ErgoBox.R4 -> ByteArrayConstant(buyOrderBox.id))
    )

    val sellOrders = sellOrderParams.map { p =>
      ErgoBox(
        value = p.tokenAmount * dexFeePerToken,
        ergoTree = DexLimitOrderContracts
          .sellerContractInstance(
            DexSellerContractParameters(sellerPk, tokenId, p.tokenPrice, dexFeePerToken)
          )
          .ergoTree,
        creationHeight   = p.creationHeight,
        additionalTokens = Seq((Digest32 @@ tokenId, p.tokenAmount)),
        additionalRegisters = Map(
          ErgoBox.R4 -> ByteArrayConstant(tokenId),
          ErgoBox.R5 -> LongConstant(p.tokenPrice),
          ErgoBox.R6 -> LongConstant(dexFeePerToken)
        )
      )
    }

    val boxesToSpend = sellOrders ++ IndexedSeq(buyOrderBox)
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
      self                = buyOrderBox
    )

    verifier
      .verify(buyOrderBox.ergoTree, context, ProverResult.empty, fakeMessage)
      .get
      ._1
  }

  property("same height, same price") {
    val buyerTokenPrice        = 20000000L
    val buyerTokenAmount       = 100L
    val buyOrderCreationHeight = 100

    checkSpread(
      buyerTokenPrice,
      buyerTokenAmount,
      buyOrderCreationHeight,
      IndexedSeq(
        SellOrder(buyerTokenPrice, buyerTokenAmount / 2, buyOrderCreationHeight),
        SellOrder(buyerTokenPrice, buyerTokenAmount / 2, buyOrderCreationHeight)
      ),
      receivedSpread = 0L
    ) shouldBe true
  }

  property("older buy orders, same price") {
    val buyerTokenPrice        = 20000000L
    val buyerTokenAmount       = 100L
    val buyOrderCreationHeight = 100

    checkSpread(
      buyerTokenPrice,
      buyerTokenAmount,
      buyOrderCreationHeight,
      IndexedSeq(
        SellOrder(buyerTokenPrice, buyerTokenAmount / 2, buyOrderCreationHeight + 1),
        SellOrder(buyerTokenPrice, buyerTokenAmount / 2, buyOrderCreationHeight + 1)
      ),
      receivedSpread = 0L
    ) shouldBe true

  }

  property("older buy orders, different price") {
    val buyerTokenPrice        = 20000000L
    val buyerTokenAmount       = 100L
    val buyOrderCreationHeight = 100

    checkSpread(
      buyerTokenPrice,
      buyerTokenAmount,
      buyOrderCreationHeight,
      IndexedSeq(
        SellOrder(buyerTokenPrice - 2, buyerTokenAmount / 2, buyOrderCreationHeight + 1),
        SellOrder(buyerTokenPrice - 1, buyerTokenAmount / 2, buyOrderCreationHeight + 1)
      ),
      receivedSpread = (buyerTokenAmount / 2) * 1 + (buyerTokenAmount / 2) * 2
    ) shouldBe true
  }

  property("mixed buy orders height") {
    val buyerTokenPrice        = 20000000L
    val buyerTokenAmount       = 99L
    val buyOrderCreationHeight = 100

    val sellOrderWithSpread2 =
      SellOrder(buyerTokenPrice - 2, buyerTokenAmount / 3, buyOrderCreationHeight + 1)

    val sellOrderWithSpread1 =
      SellOrder(buyerTokenPrice - 1, buyerTokenAmount / 3, buyOrderCreationHeight + 1)

    // Although the price is the highest, spread is not ours (height < sell order's height)
    val sellOrderWithSpread0 =
      SellOrder(buyerTokenPrice - 5, buyerTokenAmount / 3, buyOrderCreationHeight - 1)

    // fine (box with lost spread is last)
    checkSpread(
      buyerTokenPrice,
      buyerTokenAmount,
      buyOrderCreationHeight,
      IndexedSeq(
        sellOrderWithSpread2,
        sellOrderWithSpread1,
        sellOrderWithSpread0
      ),
      receivedSpread = (buyerTokenAmount / 2) * 1 + (buyerTokenAmount / 2) * 2
    ) shouldBe true

    // fail (box with lost spread is not last)
    checkSpread(
      buyerTokenPrice,
      buyerTokenAmount,
      buyOrderCreationHeight,
      IndexedSeq(
        sellOrderWithSpread0,
        sellOrderWithSpread2,
        sellOrderWithSpread1
      ),
      receivedSpread = (buyerTokenAmount / 2) * 1 + (buyerTokenAmount / 2) * 2
    ) shouldBe false

    // also fail (box with lost spread is not last)
    checkSpread(
      buyerTokenPrice,
      buyerTokenAmount,
      buyOrderCreationHeight,
      IndexedSeq(
        sellOrderWithSpread2,
        sellOrderWithSpread0,
        sellOrderWithSpread1
      ),
      receivedSpread = (buyerTokenAmount / 2) * 1 + (buyerTokenAmount / 2) * 2
    ) shouldBe false
  }

}
