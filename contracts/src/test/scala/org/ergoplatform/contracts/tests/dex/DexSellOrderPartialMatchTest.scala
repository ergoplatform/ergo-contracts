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

class DexSellOrderPartialMatchTest
  extends SigmaTestingCommons
  with ObjectGenerators
  with DexTestingCommons {

  property("sell order partial matching") {
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
        val partialMatchTokenAmount = tokenAmount / 2

        val buyOrderContractParams =
          DexBuyerContractParameters(buyerPk, tokenId, tokenPrice, dexFeePerToken)

        val buyOrderContract =
          DexLimitOrderContracts.buyerContractInstance(buyOrderContractParams)

        val buyOrderBox = ErgoBox(
          value          = partialMatchTokenAmount * (tokenPrice + dexFeePerToken),
          ergoTree       = buyOrderContract.ergoTree,
          creationHeight = 0,
          additionalRegisters = Map(
            ErgoBox.R4 -> ByteArrayConstant(tokenId),
            ErgoBox.R5 -> LongConstant(tokenPrice),
            ErgoBox.R6 -> LongConstant(dexFeePerToken)
          )
        )

        val sellOrderContractParams =
          DexSellerContractParameters(sellerPk, tokenId, tokenPrice, dexFeePerToken)

        val sellOrderContract =
          DexLimitOrderContracts.sellerContractInstance(sellOrderContractParams)

        val sellOrderBox = ErgoBox(
          value            = tokenAmount * dexFeePerToken,
          ergoTree         = sellOrderContract.ergoTree,
          creationHeight   = 0,
          additionalTokens = Seq((Digest32 @@ tokenId, tokenAmount)),
          additionalRegisters = Map(
            ErgoBox.R4 -> ByteArrayConstant(tokenId),
            ErgoBox.R5 -> LongConstant(tokenPrice)
          )
        )

        val residualSellOrderBox = ErgoBox(
          value          = (tokenAmount - partialMatchTokenAmount) * dexFeePerToken,
          ergoTree       = sellOrderContract.ergoTree,
          creationHeight = 0,
          additionalTokens =
            Seq((Digest32 @@ tokenId, tokenAmount - partialMatchTokenAmount)),
          additionalRegisters = Map(
            ErgoBox.R4 -> ByteArrayConstant(tokenId),
            ErgoBox.R5 -> LongConstant(tokenPrice),
            ErgoBox.R6 -> LongConstant(dexFeePerToken),
            ErgoBox.R7 -> ByteArrayConstant(sellOrderBox.id)
          )
        )

        val returnBox = ErgoBox(
          value               = partialMatchTokenAmount * tokenPrice,
          ergoTree            = sellerPk,
          creationHeight      = 0,
          additionalRegisters = Map(ErgoBox.R4 -> ByteArrayConstant(sellOrderBox.id))
        )

        val tx = new ErgoLikeTransaction(
          IndexedSeq(buyOrderBox, sellOrderBox).map(b => Input(b.id, ProverResult.empty)),
          IndexedSeq(),
          IndexedSeq(returnBox, residualSellOrderBox)
        )

        val context = ErgoLikeContextTesting(
          currentHeight       = 0,
          lastBlockUtxoRoot   = AvlTreeData.dummy,
          minerPubkey         = ErgoLikeContextTesting.dummyPubkey,
          boxesToSpend        = IndexedSeq(sellOrderBox, buyOrderBox),
          spendingTransaction = tx,
          self                = sellOrderBox
        )

        val (res, cost) = verifier
          .verify(sellOrderBox.ergoTree, context, ProverResult.empty, fakeMessage)
          .get
        res shouldBe true
        println(s"sell order contract cost: $cost")
        cost should be < sellerContractCostLimit
    }
  }

  property("sell order partial matching without residual sell order box") {
    val verifier = new ErgoLikeTestInterpreter
    val buyerPk =
      (new ContextEnrichingTestProvingInterpreter).dlogSecrets.head.publicImage
    val sellerPk =
      (new ContextEnrichingTestProvingInterpreter).dlogSecrets.head.publicImage

    val tokenId        = tokenIdGen.sample.get
    val tokenPrice     = 20000000L
    val dexFeePerToken = 1000000L
    val tokenAmount    = 99L

    val partialMatchTokenAmount = tokenAmount / 2

    val buyOrderContractParams =
      DexBuyerContractParameters(buyerPk, tokenId, tokenPrice, dexFeePerToken)

    val buyOrderContract =
      DexLimitOrderContracts.buyerContractInstance(buyOrderContractParams)

    val buyOrderBox = ErgoBox(
      value          = partialMatchTokenAmount * (tokenPrice + dexFeePerToken),
      ergoTree       = buyOrderContract.ergoTree,
      creationHeight = 0,
      additionalRegisters = Map(
        ErgoBox.R4 -> ByteArrayConstant(tokenId),
        ErgoBox.R5 -> LongConstant(tokenPrice),
        ErgoBox.R6 -> LongConstant(dexFeePerToken)
      )
    )

    val sellOrderContractParams =
      DexSellerContractParameters(sellerPk, tokenId, tokenPrice, dexFeePerToken)

    val sellOrderContract =
      DexLimitOrderContracts.sellerContractInstance(sellOrderContractParams)

    val sellOrderBox = ErgoBox(
      value            = tokenAmount * dexFeePerToken,
      ergoTree         = sellOrderContract.ergoTree,
      creationHeight   = 0,
      additionalTokens = Seq((Digest32 @@ tokenId, tokenAmount)),
      additionalRegisters = Map(
        ErgoBox.R4 -> ByteArrayConstant(tokenId),
        ErgoBox.R5 -> LongConstant(tokenPrice)
      )
    )

    val returnBox = ErgoBox(
      value               = partialMatchTokenAmount * tokenPrice,
      ergoTree            = sellerPk,
      creationHeight      = 0,
      additionalRegisters = Map(ErgoBox.R4 -> ByteArrayConstant(sellOrderBox.id))
    )

    val tx = new ErgoLikeTransaction(
      IndexedSeq(buyOrderBox, sellOrderBox).map(b => Input(b.id, ProverResult.empty)),
      IndexedSeq(),
      IndexedSeq(returnBox)
    )

    val context = ErgoLikeContextTesting(
      currentHeight       = 0,
      lastBlockUtxoRoot   = AvlTreeData.dummy,
      minerPubkey         = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend        = IndexedSeq(sellOrderBox, buyOrderBox),
      spendingTransaction = tx,
      self                = sellOrderBox
    )

    val (res, cost) = verifier
      .verify(sellOrderBox.ergoTree, context, ProverResult.empty, fakeMessage)
      .get
    res shouldBe false
  }

  property("sell order partial matching with incorrect residual sell order box") {
    val verifier = new ErgoLikeTestInterpreter
    val buyerPk =
      (new ContextEnrichingTestProvingInterpreter).dlogSecrets.head.publicImage
    val sellerPk =
      (new ContextEnrichingTestProvingInterpreter).dlogSecrets.head.publicImage

    val tokenId        = tokenIdGen.sample.get
    val tokenPrice     = 20000000L
    val dexFeePerToken = 1000000L
    val tokenAmount    = 99L

    val partialMatchTokenAmount = tokenAmount / 2

    val buyOrderContractParams =
      DexBuyerContractParameters(buyerPk, tokenId, tokenPrice, dexFeePerToken)

    val buyOrderContract =
      DexLimitOrderContracts.buyerContractInstance(buyOrderContractParams)

    val buyOrderBox = ErgoBox(
      value          = partialMatchTokenAmount * (tokenPrice + dexFeePerToken),
      ergoTree       = buyOrderContract.ergoTree,
      creationHeight = 0,
      additionalRegisters = Map(
        ErgoBox.R4 -> ByteArrayConstant(tokenId),
        ErgoBox.R5 -> LongConstant(tokenPrice),
        ErgoBox.R6 -> LongConstant(dexFeePerToken)
      )
    )

    val sellOrderContractParams =
      DexSellerContractParameters(sellerPk, tokenId, tokenPrice, dexFeePerToken)

    val sellOrderContract =
      DexLimitOrderContracts.sellerContractInstance(sellOrderContractParams)

    val sellOrderBox = ErgoBox(
      value            = tokenAmount * dexFeePerToken,
      ergoTree         = sellOrderContract.ergoTree,
      creationHeight   = 0,
      additionalTokens = Seq((Digest32 @@ tokenId, tokenAmount)),
      additionalRegisters = Map(
        ErgoBox.R4 -> ByteArrayConstant(tokenId),
        ErgoBox.R5 -> LongConstant(tokenPrice)
      )
    )

    val returnBox = ErgoBox(
      value               = partialMatchTokenAmount * tokenPrice,
      ergoTree            = sellerPk,
      creationHeight      = 0,
      additionalRegisters = Map(ErgoBox.R4 -> ByteArrayConstant(sellOrderBox.id))
    )

    def verifyWithResidualOrder(residualOrder: ErgoBox): Boolean =
      verifier
        .verify(
          sellOrderBox.ergoTree,
          ErgoLikeContextTesting(
            currentHeight     = 0,
            lastBlockUtxoRoot = AvlTreeData.dummy,
            minerPubkey       = ErgoLikeContextTesting.dummyPubkey,
            boxesToSpend      = IndexedSeq(sellOrderBox, buyOrderBox),
            spendingTransaction = new ErgoLikeTransaction(
              IndexedSeq(buyOrderBox, sellOrderBox).map(b =>
                Input(b.id, ProverResult.empty)
              ),
              IndexedSeq(),
              IndexedSeq(returnBox, residualOrder)
            ),
            self = sellOrderBox
          ),
          ProverResult.empty,
          fakeMessage
        )
        .get
        ._1

    // incorrect value
    verifyWithResidualOrder(
      ErgoBox(
        value          = (tokenAmount - partialMatchTokenAmount) * dexFeePerToken - 1,
        ergoTree       = sellOrderContract.ergoTree,
        creationHeight = 0,
        additionalTokens =
          Seq((Digest32 @@ tokenId, tokenAmount - partialMatchTokenAmount)),
        additionalRegisters = Map(
          ErgoBox.R4 -> ByteArrayConstant(tokenId),
          ErgoBox.R5 -> LongConstant(tokenPrice),
          ErgoBox.R6 -> LongConstant(dexFeePerToken),
          ErgoBox.R7 -> ByteArrayConstant(sellOrderBox.id)
        )
      )
    ) shouldBe false

    // incorrect residual token id
    verifyWithResidualOrder(
      ErgoBox(
        value          = (tokenAmount - partialMatchTokenAmount) * dexFeePerToken,
        ergoTree       = sellOrderContract.ergoTree,
        creationHeight = 0,
        additionalTokens =
          Seq((Digest32 @@ tokenIdGen.sample.get, tokenAmount - partialMatchTokenAmount)),
        additionalRegisters = Map(
          ErgoBox.R4 -> ByteArrayConstant(tokenId),
          ErgoBox.R5 -> LongConstant(tokenPrice),
          ErgoBox.R6 -> LongConstant(dexFeePerToken),
          ErgoBox.R7 -> ByteArrayConstant(sellOrderBox.id)
        )
      )
    ) shouldBe false

    // incorrect residual token amount
    verifyWithResidualOrder(
      ErgoBox(
        value          = (tokenAmount - partialMatchTokenAmount) * dexFeePerToken,
        ergoTree       = sellOrderContract.ergoTree,
        creationHeight = 0,
        additionalTokens =
          Seq((Digest32 @@ tokenId, tokenAmount - partialMatchTokenAmount - 1)),
        additionalRegisters = Map(
          ErgoBox.R4 -> ByteArrayConstant(tokenId),
          ErgoBox.R5 -> LongConstant(tokenPrice),
          ErgoBox.R6 -> LongConstant(dexFeePerToken),
          ErgoBox.R7 -> ByteArrayConstant(sellOrderBox.id)
        )
      )
    ) shouldBe false

    // incorrect token id in R4
    verifyWithResidualOrder(
      ErgoBox(
        value          = (tokenAmount - partialMatchTokenAmount) * dexFeePerToken,
        ergoTree       = sellOrderContract.ergoTree,
        creationHeight = 0,
        additionalTokens =
          Seq((Digest32 @@ tokenId, tokenAmount - partialMatchTokenAmount)),
        additionalRegisters = Map(
          ErgoBox.R4 -> ByteArrayConstant(tokenIdGen.sample.get),
          ErgoBox.R5 -> LongConstant(tokenPrice),
          ErgoBox.R6 -> LongConstant(dexFeePerToken),
          ErgoBox.R7 -> ByteArrayConstant(sellOrderBox.id)
        )
      )
    ) shouldBe false

    // incorrect token price in R5
    verifyWithResidualOrder(
      ErgoBox(
        value          = (tokenAmount - partialMatchTokenAmount) * dexFeePerToken,
        ergoTree       = sellOrderContract.ergoTree,
        creationHeight = 0,
        additionalTokens =
          Seq((Digest32 @@ tokenId, tokenAmount - partialMatchTokenAmount)),
        additionalRegisters = Map(
          ErgoBox.R4 -> ByteArrayConstant(tokenId),
          ErgoBox.R5 -> LongConstant(tokenPrice - 1),
          ErgoBox.R6 -> LongConstant(dexFeePerToken),
          ErgoBox.R7 -> ByteArrayConstant(sellOrderBox.id)
        )
      )
    ) shouldBe false

    // incorrect dex fee per token in R6
    verifyWithResidualOrder(
      ErgoBox(
        value          = (tokenAmount - partialMatchTokenAmount) * dexFeePerToken,
        ergoTree       = sellOrderContract.ergoTree,
        creationHeight = 0,
        additionalTokens =
          Seq((Digest32 @@ tokenId, tokenAmount - partialMatchTokenAmount)),
        additionalRegisters = Map(
          ErgoBox.R4 -> ByteArrayConstant(tokenId),
          ErgoBox.R5 -> LongConstant(tokenPrice),
          ErgoBox.R6 -> LongConstant(dexFeePerToken - 1),
          ErgoBox.R7 -> ByteArrayConstant(sellOrderBox.id)
        )
      )
    ) shouldBe false

    // incorrect self id in R7
    verifyWithResidualOrder(
      ErgoBox(
        value          = (tokenAmount - partialMatchTokenAmount) * dexFeePerToken,
        ergoTree       = sellOrderContract.ergoTree,
        creationHeight = 0,
        additionalTokens =
          Seq((Digest32 @@ tokenId, tokenAmount - partialMatchTokenAmount)),
        additionalRegisters = Map(
          ErgoBox.R4 -> ByteArrayConstant(tokenId),
          ErgoBox.R5 -> LongConstant(tokenPrice),
          ErgoBox.R6 -> LongConstant(dexFeePerToken),
          ErgoBox.R7 -> ByteArrayConstant(boxIdGen.sample.get)
        )
      )
    ) shouldBe false

  }
}
