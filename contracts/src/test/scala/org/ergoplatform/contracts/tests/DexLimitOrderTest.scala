package org.ergoplatform.contracts.tests

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

class DexLimitOrderTest extends SigmaTestingCommons with ObjectGenerators {

  implicit lazy val IR: TestingIRContext = new TestingIRContext

  // private def ctx(
  //   height: Int,
  //   tx: ErgoLikeTransaction,
  //   selfBox: ErgoBox
  // ): ErgoLikeContext =
  //   ErgoLikeContextTesting(
  //     currentHeight       = height,
  //     lastBlockUtxoRoot   = AvlTreeData.dummy,
  //     minerPubkey         = ErgoLikeContextTesting.dummyPubkey,
  //     boxesToSpend        = IndexedSeq(selfBox),
  //     spendingTransaction = tx,
  //     self                = selfBox
  //   )

  // implicit private def toSigmaContext(ergoCtx: ErgoLikeContext): special.sigma.Context =
  //   ergoCtx.toSigmaContext(IR, false)

  property("buy order total matching") {
    val verifier = new ErgoLikeTestInterpreter
    val prover   = new ContextEnrichingTestProvingInterpreter
    val pubkey   = prover.dlogSecrets.head.publicImage

    // fail fast without shrinking
    import org.scalacheck.Shrink.shrinkAny

    val maxTokenAmount = 1000L

    forAll(
      tokenIdGen,
      Gen.chooseNum(2L, Long.MaxValue / (2 * maxTokenAmount)).suchThat(_ > 1L),
      Gen.chooseNum(2L, Long.MaxValue / (2 * maxTokenAmount)).suchThat(_ > 1L),
      Gen.chooseNum(1L, maxTokenAmount)
    ) {
      case (tokenId, tokenPrice, dexFeePerToken, tokenAmount) =>
        val buyOrderContractParams =
          DexBuyerContractParameters(pubkey, tokenId, tokenPrice, dexFeePerToken)

        val buyOrderContract =
          DexLimitOrderContracts.buyerContractInstance(buyOrderContractParams)

        val selfBox = ErgoBox(
          value          = tokenAmount * (tokenPrice + dexFeePerToken),
          ergoTree       = buyOrderContract.ergoTree,
          creationHeight = 0
        )

        val sellOrderContractParams =
          DexSellerContractParameters(pubkey, tokenId, tokenPrice, dexFeePerToken)

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
          value               = 1, // TODO: min fee?
          ergoTree            = pubkey,
          creationHeight      = 0,
          additionalTokens    = Seq((Digest32 @@ tokenId, tokenAmount)),
          additionalRegisters = Map(ErgoBox.R4 -> ByteArrayConstant(selfBox.id))
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
          boxesToSpend        = IndexedSeq(sellOrderBox, selfBox),
          spendingTransaction = tx,
          self                = selfBox
        )

        val (res, cost) = verifier
          .verify(selfBox.ergoTree, context, ProverResult.empty, fakeMessage)
          .get
        res shouldBe true
        println(s"buy order contract cost: $cost")
        cost should be < 110000L
    }
  }
}
