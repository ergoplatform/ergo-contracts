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

class DexOrderCancellationTest
  extends SigmaTestingCommons
  with ObjectGenerators
  with DexTestingCommons {

  property("buy order cancellation") {
    val prover   = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    val buyerPk  = prover.dlogSecrets.head.publicImage

    val tokenId        = tokenIdGen.sample.get
    val tokenPrice     = 20000000L
    val dexFeePerToken = 1000000L
    val tokenAmount    = 99L

    val buyOrderContractParams =
      DexBuyerContractParameters(buyerPk, tokenId, tokenPrice, dexFeePerToken)

    val buyOrderContract =
      DexLimitOrderContracts.buyerContractInstance(buyOrderContractParams)

    val selfBox = ErgoBox(
      value          = tokenAmount * (tokenPrice + dexFeePerToken),
      ergoTree       = buyOrderContract.ergoTree,
      creationHeight = 0
    )

    val returnBox = ErgoBox(
      value          = selfBox.value,
      ergoTree       = buyerPk,
      creationHeight = 0
    )

    val tx = new ErgoLikeTransaction(
      IndexedSeq(selfBox).map(b => Input(b.id, ProverResult.empty)),
      IndexedSeq(),
      IndexedSeq(returnBox)
    )

    val context = ErgoLikeContextTesting(
      currentHeight       = 0,
      lastBlockUtxoRoot   = AvlTreeData.dummy,
      minerPubkey         = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend        = IndexedSeq(selfBox),
      spendingTransaction = tx,
      self                = selfBox
    )

    val pr = prover.prove(returnBox.ergoTree, context, fakeMessage).get

    val (res, cost) = verifier
      .verify(selfBox.ergoTree, context, pr, fakeMessage)
      .get
    res shouldBe true
  }

  property("sell order cancellation") {
    val verifier = new ErgoLikeTestInterpreter
    val prover   = new ContextEnrichingTestProvingInterpreter
    val sellerPk =
      prover.dlogSecrets.head.publicImage

    val tokenId        = tokenIdGen.sample.get
    val tokenPrice     = 20000000L
    val dexFeePerToken = 1000000L
    val tokenAmount    = 99L

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
      value            = tokenAmount * dexFeePerToken,
      ergoTree         = sellerPk,
      creationHeight   = 0,
      additionalTokens = Seq((Digest32 @@ tokenId, tokenAmount))
    )

    val tx = new ErgoLikeTransaction(
      IndexedSeq(sellOrderBox).map(b => Input(b.id, ProverResult.empty)),
      IndexedSeq(),
      IndexedSeq(returnBox)
    )

    val context = ErgoLikeContextTesting(
      currentHeight       = 0,
      lastBlockUtxoRoot   = AvlTreeData.dummy,
      minerPubkey         = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend        = IndexedSeq(sellOrderBox),
      spendingTransaction = tx,
      self                = sellOrderBox
    )

    val pr = prover.prove(returnBox.ergoTree, context, fakeMessage).get

    val (res, cost) = verifier
      .verify(sellOrderBox.ergoTree, context, pr, fakeMessage)
      .get
    res shouldBe true
  }
}
