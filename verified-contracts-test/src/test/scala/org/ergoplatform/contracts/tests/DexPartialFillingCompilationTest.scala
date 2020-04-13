package org.ergoplatform.contracts.tests

import org.ergoplatform.MiscGenerators
import org.ergoplatform.contracts.{DexBuyerContractParameters, DexPartialFillingContracts}
import org.scalacheck.Arbitrary.arbLong
import org.scalacheck.Gen
import sigmastate.eval.Extensions._
import sigmastate.helpers.SigmaTestingCommons

import scala.language.implicitConversions

class DexPartialFillingCompilationTest extends SigmaTestingCommons with MiscGenerators {

  property("ErgoScript buyer contract parameters extraction and template test") {
    forAll(
      tokenIdGen,
      Gen.chooseNum(2L, Long.MaxValue).suchThat(_ > 1L),
      Gen.chooseNum(2L, Long.MaxValue).suchThat(_ > 1L),
      proveDlogGen
    ) {
      case (tokenId, tokenPrice, dexFeePerToken, proveDlogPk) =>
        val params =
          DexBuyerContractParameters(proveDlogPk, tokenId, tokenPrice, dexFeePerToken)
        val ergoTree = DexPartialFillingContracts.buyerContractInstance(params).ergoTree
        DexPartialFillingContracts
          .parseBuyerContractParameters(ergoTree)
          .get shouldEqual params

        DexPartialFillingContracts.buyerContractErgoTreeTemplate shouldEqual DexPartialFillingContracts
          .ergoTreeTemplateBytes(ergoTree)
    }
  }

}
