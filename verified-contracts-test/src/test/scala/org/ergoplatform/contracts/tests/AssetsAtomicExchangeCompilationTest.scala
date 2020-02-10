package org.ergoplatform.contracts.tests

import org.ergoplatform.ErgoBox.{R2, R4}
import org.ergoplatform.contracts.AssetsAtomicExchangeCompilation
import org.ergoplatform.{
  ErgoBox,
  ErgoLikeContext,
  ErgoLikeTransaction,
  Height,
  MiscGenerators,
  Outputs,
  Self
}
import org.scalacheck.Arbitrary.arbLong
import scorex.crypto.hash.Digest32
import sigmastate.Values.{
  BlockValue,
  ByteArrayConstant,
  IntConstant,
  LongConstant,
  SigmaPropConstant,
  ValDef,
  ValUse,
  Value
}
import sigmastate.eval.CSigmaProp
import sigmastate.helpers.{
  ContextEnrichingTestProvingInterpreter,
  ErgoLikeContextTesting,
  ErgoLikeTestInterpreter,
  SigmaTestingCommons
}
import sigmastate.utxo.{
  ByIndex,
  ExtractAmount,
  ExtractId,
  ExtractRegisterAs,
  ExtractScriptBytes,
  OptionIsDefined,
  SelectField,
  SigmaPropBytes,
  SizeOf
}
import org.ergoplatform.sigma.verified.VerifiedTypeConverters._
import sigmastate.{utxo, _}
import special.collection.{Coll, CollOverArray}
import special.sigma.SigmaProp
import sigmastate.eval.Extensions._
import sigmastate.interpreter.ProverResult

import scala.language.implicitConversions

class AssetsAtomicExchangeCompilationTest
  extends SigmaTestingCommons
  with MiscGenerators {

  implicit lazy val IR: TestingIRContext = new TestingIRContext

  private def ctx(
    height: Int,
    tx: ErgoLikeTransaction,
    selfBox: ErgoBox = fakeSelf
  ): ErgoLikeContext =
    ErgoLikeContextTesting(
      currentHeight       = height,
      lastBlockUtxoRoot   = AvlTreeData.dummy,
      minerPubkey         = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend        = IndexedSeq(selfBox),
      spendingTransaction = tx,
      self                = selfBox
    )

  implicit private def toSigmaContext(ergoCtx: ErgoLikeContext): special.sigma.Context =
    ergoCtx.toSigmaContext(IR, false)

  property("buyer contract ergo tree") {
    forAll(tokenIdGen.map(_.toColl), arbLong.arbitrary, proveDlogGen) {
      case (tokenId, tokenAmount, proveDlogPk) =>
        val pk: SigmaProp = CSigmaProp(proveDlogPk)
        val c =
          AssetsAtomicExchangeCompilation.buyerContractInstance(tokenId, tokenAmount, pk)
        val expectedProp = SigmaOr(
          Seq(
            SigmaPropConstant(proveDlogPk),
            BinAnd(
              BinAnd(
                GT(
                  SizeOf(Outputs),
                  IntConstant(0)
                ),
                OptionIsDefined(
                  ExtractRegisterAs(
                    ByIndex(Outputs, IntConstant(0), None),
                    R4,
                    SOption(SCollectionType(SByte))
                  )
                )
              ),
              BlockValue(
                Vector(
                  // tokens
                  ValDef(
                    1,
                    List(),
                    ExtractRegisterAs(
                      ByIndex(Outputs, IntConstant(0), None),
                      R2,
                      SOption(SCollectionType(STuple(SCollectionType(SByte), SLong)))
                    ).get
                  ),
                  ValDef(
                    2,
                    List(),
                    BinAnd(
                      BinAnd(
                        GT(
                          SizeOf(
                            ValUse(
                              1,
                              SCollectionType(STuple(SCollectionType(SByte), SLong))
                            )
                          ),
                          IntConstant(0)
                        ),
                        EQ(
                          SelectField(
                            ByIndex(
                              ValUse(
                                1,
                                SCollectionType(STuple(SCollectionType(SByte), SLong))
                              ),
                              IntConstant(0),
                              None
                            ),
                            1
                          ),
                          ByteArrayConstant(tokenId)
                        )
                      ),
                      GE(
                        SelectField(
                          ByIndex(
                            ValUse(
                              1,
                              SCollectionType(STuple(SCollectionType(SByte), SLong))
                            ),
                            IntConstant(0),
                            None
                          ),
                          2
                        ),
                        LongConstant(tokenAmount)
                      )
                    )
                  ),
                  ValDef(
                    3,
                    List(),
                    EQ(
                      ExtractRegisterAs(
                        ByIndex(Outputs, IntConstant(0), None),
                        R4,
                        SOption(SCollectionType(SByte))
                      ).get,
                      ExtractId(Self)
                    )
                  )
                ),
                BinAnd(
                  BinAnd(
                    ValUse(2, SBoolean),
                    EQ(
                      ExtractScriptBytes(ByIndex(Outputs, IntConstant(0), None)),
                      SigmaPropBytes(SigmaPropConstant(proveDlogPk))
                    )
                  ),
                  ValUse(3, SBoolean)
                )
              ).asInstanceOf[Value[SBoolean.type]]
            ).toSigmaProp
          )
        )
        assert(c.prop == expectedProp)
    }
  }

  property("seller contract ergo tree") {
    forAll(arbLong.arbitrary, proveDlogGen) {
      case (ergAmount, proveDlogPk) =>
        val pk: SigmaProp = CSigmaProp(proveDlogPk)
        val c             = AssetsAtomicExchangeCompilation.sellerContractInstance(ergAmount, pk)
        val expectedProp = SigmaOr(
          Seq(
            SigmaPropConstant(proveDlogPk),
            BoolToSigmaProp(
              BinAnd(
                BinAnd(
                  GT(SizeOf(Outputs), IntConstant(1)),
                  OptionIsDefined(
                    ExtractRegisterAs(
                      ByIndex(Outputs, IntConstant(1), None),
                      R4,
                      SOption(SCollectionType(SByte))
                    )
                  )
                ),
                BlockValue(
                  Vector(
                    ValDef(
                      1,
                      List(),
                      EQ(
                        ExtractRegisterAs(
                          ByIndex(Outputs, IntConstant(1), None),
                          R4,
                          SOption(SCollectionType(SByte))
                        ).get,
                        ExtractId(Self)
                      )
                    )
                  ),
                  BinAnd(
                    BinAnd(
                      GE(
                        ExtractAmount(ByIndex(Outputs, IntConstant(1), None)),
                        LongConstant(ergAmount)
                      ),
                      ValUse(1, SBoolean)
                    ),
                    EQ(
                      ExtractScriptBytes(ByIndex(Outputs, IntConstant(1), None)),
                      SigmaPropBytes(SigmaPropConstant(proveDlogPk))
                    )
                  )
                ).asInstanceOf[Value[SBoolean.type]]
              )
            )
          )
        )
        assert(c.prop == expectedProp)
    }
  }

  property("buyer contract, buyer claim") {
    val prover      = new ContextEnrichingTestProvingInterpreter
    val verifier    = new ErgoLikeTestInterpreter
    val tokenId     = tokenIdGen.sample.get
    val tokenAmount = 100L
    val pubkey      = prover.dlogSecrets.head.publicImage

    val pk: SigmaProp = CSigmaProp(pubkey)
    val c = AssetsAtomicExchangeCompilation.buyerContractInstance(
      tokenId.toColl,
      tokenAmount,
      pk
    )
    val tree = c.ergoTree

    val spendingTransaction = createTransaction(
      IndexedSeq(
        ErgoBox(1, pubkey, 0, Seq((tokenIdGen.sample.get, 0))) // non-empty tokens as a workaround for
        // https://github.com/ScorexFoundation/sigmastate-interpreter/issues/628
      )
    )
    val context = ctx(50, spendingTransaction)

    val pr = prover.prove(tree, context, fakeMessage).get
    verifier.verify(tree, context, pr, fakeMessage).get._1 shouldBe true
  }

  property("buyer contract, no tokens") {
    val verifier           = new ErgoLikeTestInterpreter
    val prover             = new ContextEnrichingTestProvingInterpreter
    val pubkey             = prover.dlogSecrets.head.publicImage
    val buyerPk: SigmaProp = CSigmaProp(pubkey)
    val txNoTokens         = createTransaction(IndexedSeq(ErgoBox(1, TrivialProp.TrueProp, 0)))

    forAll(tokenIdGen.map(_.toColl), arbLong.arbitrary) {
      case (tokenId, tokenAmount) =>
        val contract = AssetsAtomicExchangeCompilation.buyerContractInstance(
          tokenId     = tokenId,
          tokenAmount = tokenAmount,
          pkA         = buyerPk
        )

        val contextBeforeDeadline = ctx(50, txNoTokens)
        contract.scalaFunc(contextBeforeDeadline) shouldEqual buyerPk
        verifier
          .verify(
            contract.ergoTree,
            contextBeforeDeadline,
            ProverResult.empty,
            fakeMessage
          )
          .isSuccess shouldBe false
    }
  }

  property("buyer contract, tokens") {
    val verifier           = new ErgoLikeTestInterpreter
    val prover             = new ContextEnrichingTestProvingInterpreter
    val pubkey             = prover.dlogSecrets.head.publicImage
    val buyerPk: SigmaProp = CSigmaProp(pubkey)

    forAll(tokenIdGen.map(_.toColl), arbLong.arbitrary) {
      case (tokenId, tokenAmount) =>
        val txWithTokens = createTransaction(
          IndexedSeq(
            ErgoBox(
              value               = 1,
              ergoTree            = pubkey,
              creationHeight      = 0,
              additionalTokens    = Seq((Digest32 @@ tokenId.toArray, tokenAmount)),
              additionalRegisters = Map(ErgoBox.R4 -> ByteArrayConstant(fakeSelf.id))
            )
          )
        )

        val contract = AssetsAtomicExchangeCompilation.buyerContractInstance(
          tokenId     = tokenId,
          tokenAmount = tokenAmount,
          pkA         = buyerPk
        )

        val ctxBeforeDeadline = ctx(50, txWithTokens)
        contract.scalaFunc(ctxBeforeDeadline) shouldEqual CSigmaProp(TrivialProp.TrueProp)
        verifier
          .verify(contract.ergoTree, ctxBeforeDeadline, ProverResult.empty, fakeMessage)
          .get
          ._1 shouldEqual true
    }
  }

  property("seller contract, seller claim") {
    val prover    = new ContextEnrichingTestProvingInterpreter
    val verifier  = new ErgoLikeTestInterpreter
    val ergAmount = 100L
    val pubkey    = prover.dlogSecrets.head.publicImage

    val pk: SigmaProp = CSigmaProp(pubkey)
    val c             = AssetsAtomicExchangeCompilation.sellerContractInstance(ergAmount, pk)
    val tree          = c.ergoTree

    val spendingTransaction = createTransaction(
      IndexedSeq(
        ErgoBox(value = 1, ergoTree = TrivialProp.TrueProp, creationHeight = 0),
        // second box as a workaround for costing issue
        // https://github.com/ScorexFoundation/sigmastate-interpreter/issues/628
        ErgoBox(
          value          = 1,
          ergoTree       = TrivialProp.TrueProp, // any address
          creationHeight = 0
        )
      )
    )
    val context = ctx(50, spendingTransaction)

    val pr = prover.prove(tree, context, fakeMessage).get
    verifier.verify(tree, context, pr, fakeMessage).get._1 shouldBe true
  }

  property("seller contract, no buyer") {
    val verifier            = new ErgoLikeTestInterpreter
    val prover              = new ContextEnrichingTestProvingInterpreter
    val pubkey              = prover.dlogSecrets.head.publicImage
    val sellerPk: SigmaProp = CSigmaProp(pubkey)
    val tx                  = createTransaction(IndexedSeq(ErgoBox(1, TrivialProp.TrueProp, 0)))

    forAll(arbLong.arbitrary) {
      case (ergAmount) =>
        val contract =
          AssetsAtomicExchangeCompilation.sellerContractInstance(ergAmount, sellerPk)

        val contextBeforeDeadline = ctx(50, tx)
        contract.scalaFunc(contextBeforeDeadline) shouldEqual sellerPk
        verifier
          .verify(
            contract.ergoTree,
            contextBeforeDeadline,
            ProverResult.empty,
            fakeMessage
          )
          .isSuccess shouldBe false
    }
  }

  property("seller contract, with buyer") {
    val verifier                = new ErgoLikeTestInterpreter
    val prover                  = new ContextEnrichingTestProvingInterpreter
    val sellerPk                = prover.dlogSecrets.head.publicImage
    val sellerPkProp: SigmaProp = CSigmaProp(sellerPk)

    forAll(arbLong.arbitrary) {
      case (ergAmount) =>
        val tx = createTransaction(
          IndexedSeq(
            ErgoBox(value = 1, ergoTree = TrivialProp.TrueProp, creationHeight = 0),
            ErgoBox(
              value               = ergAmount,
              ergoTree            = sellerPk,
              creationHeight      = 0,
              additionalRegisters = Map(ErgoBox.R4 -> ByteArrayConstant(fakeSelf.id))
            )
          )
        )
        val contract =
          AssetsAtomicExchangeCompilation.sellerContractInstance(ergAmount, sellerPkProp)

        val ctxBeforeDeadline = ctx(50, tx)
        contract.scalaFunc(ctxBeforeDeadline) shouldEqual CSigmaProp(TrivialProp.TrueProp)
        verifier
          .verify(contract.ergoTree, ctxBeforeDeadline, ProverResult.empty, fakeMessage)
          .get
          ._1 shouldBe true
    }
  }
}
