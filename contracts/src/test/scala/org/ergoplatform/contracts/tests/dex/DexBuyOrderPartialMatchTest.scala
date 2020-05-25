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

class DexLimitOrderTest
  extends SigmaTestingCommons
  with ObjectGenerators
  with DexTestingCommons {

  property("buy order partial matching") {
    val verifier = new ErgoLikeTestInterpreter
    val buyerPk =
      (new ContextEnrichingTestProvingInterpreter).dlogSecrets.head.publicImage
    val sellerPk =
      (new ContextEnrichingTestProvingInterpreter).dlogSecrets.head.publicImage

    // fail fast without shrinking
    import org.scalacheck.Shrink.shrinkAny

    forAll(
      tokenIdGen,
      Gen.chooseNum(2L, Long.MaxValue / (2 * maxTokenAmount)).suchThat(_ > 1L),
      Gen.chooseNum(2L, Long.MaxValue / (2 * maxTokenAmount)).suchThat(_ > 1L),
      Gen.chooseNum(2L, maxTokenAmount)
    ) {
      case (tokenId, tokenPrice, dexFeePerToken, tokenAmount) =>
        val buyOrderContractParams =
          DexBuyerContractParameters(buyerPk, tokenId, tokenPrice, dexFeePerToken)

        val buyOrderContract =
          DexLimitOrderContracts.buyerContractInstance(buyOrderContractParams)

        val buyOrderBox = ErgoBox(
          value          = tokenAmount * (tokenPrice + dexFeePerToken),
          ergoTree       = buyOrderContract.ergoTree,
          creationHeight = 0
        )

        val partialMatchTokenAmount = tokenAmount / 2

        val residualBuyOrderBox = ErgoBox(
          value          = (tokenAmount - partialMatchTokenAmount) * (tokenPrice + dexFeePerToken),
          ergoTree       = buyOrderContract.ergoTree,
          creationHeight = 0,
          additionalRegisters = Map(
            ErgoBox.R4 -> ByteArrayConstant(tokenId),
            ErgoBox.R5 -> LongConstant(tokenPrice),
            ErgoBox.R6 -> LongConstant(dexFeePerToken),
            ErgoBox.R7 -> ByteArrayConstant(buyOrderBox.id)
          )
        )

        val sellOrderContractParams =
          DexSellerContractParameters(sellerPk, tokenId, tokenPrice, dexFeePerToken)

        val sellOrderContract =
          DexLimitOrderContracts.sellerContractInstance(sellOrderContractParams)

        val sellOrderBox = ErgoBox(
          value            = partialMatchTokenAmount * dexFeePerToken,
          ergoTree         = sellOrderContract.ergoTree,
          creationHeight   = 0,
          additionalTokens = Seq((Digest32 @@ tokenId, partialMatchTokenAmount)),
          additionalRegisters = Map(
            ErgoBox.R4 -> ByteArrayConstant(tokenId),
            ErgoBox.R5 -> LongConstant(tokenPrice)
          )
        )

        val returnBox = ErgoBox(
          value               = 1,
          ergoTree            = buyerPk,
          creationHeight      = 0,
          additionalTokens    = Seq((Digest32 @@ tokenId, partialMatchTokenAmount)),
          additionalRegisters = Map(ErgoBox.R4 -> ByteArrayConstant(buyOrderBox.id))
        )

        val tx = new ErgoLikeTransaction(
          IndexedSeq(sellOrderBox).map(b => Input(b.id, ProverResult.empty)),
          IndexedSeq(),
          IndexedSeq(returnBox, residualBuyOrderBox)
        )

        val context = ErgoLikeContextTesting(
          currentHeight       = 0,
          lastBlockUtxoRoot   = AvlTreeData.dummy,
          minerPubkey         = ErgoLikeContextTesting.dummyPubkey,
          boxesToSpend        = IndexedSeq(sellOrderBox, buyOrderBox),
          spendingTransaction = tx,
          self                = buyOrderBox
        )

        val (res, cost) = verifier
          .verify(buyOrderBox.ergoTree, context, ProverResult.empty, fakeMessage)
          .get
        res shouldBe true
        println(s"buy order contract cost: $cost")
        cost should be < buyerContractCostLimit

    }
  }

  property("buy order partial match without residual buy order box") {
    val verifier = new ErgoLikeTestInterpreter
    val buyerPk =
      (new ContextEnrichingTestProvingInterpreter).dlogSecrets.head.publicImage
    val sellerPk =
      (new ContextEnrichingTestProvingInterpreter).dlogSecrets.head.publicImage

    val tokenId        = tokenIdGen.sample.get
    val tokenPrice     = 20000000L
    val dexFeePerToken = 1000000L
    val tokenAmount    = 99L

    val buyOrderContractParams =
      DexBuyerContractParameters(buyerPk, tokenId, tokenPrice, dexFeePerToken)

    val buyOrderContract =
      DexLimitOrderContracts.buyerContractInstance(buyOrderContractParams)

    val buyOrderBox = ErgoBox(
      value          = tokenAmount * (tokenPrice + dexFeePerToken),
      ergoTree       = buyOrderContract.ergoTree,
      creationHeight = 0
    )

    val partialMatchTokenAmount = tokenAmount / 2

    val residualBuyOrderBox = ErgoBox(
      value          = (tokenAmount - partialMatchTokenAmount) * (tokenPrice + dexFeePerToken),
      ergoTree       = buyOrderContract.ergoTree,
      creationHeight = 0,
      additionalRegisters = Map(
        ErgoBox.R4 -> ByteArrayConstant(tokenId),
        ErgoBox.R5 -> LongConstant(tokenPrice),
        ErgoBox.R6 -> LongConstant(dexFeePerToken),
        ErgoBox.R7 -> ByteArrayConstant(buyOrderBox.id)
      )
    )

    val sellOrderContractParams =
      DexSellerContractParameters(sellerPk, tokenId, tokenPrice, dexFeePerToken)

    val sellOrderContract =
      DexLimitOrderContracts.sellerContractInstance(sellOrderContractParams)

    val sellOrderBox = ErgoBox(
      value            = partialMatchTokenAmount * dexFeePerToken,
      ergoTree         = sellOrderContract.ergoTree,
      creationHeight   = 0,
      additionalTokens = Seq((Digest32 @@ tokenId, partialMatchTokenAmount)),
      additionalRegisters = Map(
        ErgoBox.R4 -> ByteArrayConstant(tokenId),
        ErgoBox.R5 -> LongConstant(tokenPrice)
      )
    )

    val returnBox = ErgoBox(
      value               = 1,
      ergoTree            = buyerPk,
      creationHeight      = 0,
      additionalTokens    = Seq((Digest32 @@ tokenId, partialMatchTokenAmount)),
      additionalRegisters = Map(ErgoBox.R4 -> ByteArrayConstant(buyOrderBox.id))
    )

    val txWithoutResidualBuyOrder = new ErgoLikeTransaction(
      IndexedSeq(sellOrderBox).map(b => Input(b.id, ProverResult.empty)),
      IndexedSeq(),
      IndexedSeq(returnBox)
    )

    verifier
      .verify(
        buyOrderBox.ergoTree,
        ErgoLikeContextTesting(
          currentHeight       = 0,
          lastBlockUtxoRoot   = AvlTreeData.dummy,
          minerPubkey         = ErgoLikeContextTesting.dummyPubkey,
          boxesToSpend        = IndexedSeq(sellOrderBox, buyOrderBox),
          spendingTransaction = txWithoutResidualBuyOrder,
          self                = buyOrderBox
        ),
        ProverResult.empty,
        fakeMessage
      )
      .get
      ._1 shouldBe false
  }
}
