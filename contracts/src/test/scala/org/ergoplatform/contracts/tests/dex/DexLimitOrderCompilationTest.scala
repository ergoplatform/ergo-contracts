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

class DexLimitOrderCompilationTest extends SigmaTestingCommons with ObjectGenerators {
  // fail fast without shrinking
  import org.scalacheck.Shrink.shrinkAny

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
        // println(params)
        val ergoTree = DexLimitOrderContracts.buyerContractInstance(params).ergoTree
        // println(ergoTree.constants.zipWithIndex)
        DexLimitOrderContracts
          .parseBuyerContractParameters(ergoTree)
          .get shouldEqual params

        DexLimitOrderContracts.buyerContractErgoTreeTemplate shouldEqual ergoTree.template
    }
  }

  property("ErgoScript seller contract parameters extraction and template test") {
    forAll(
      tokenIdGen,
      Gen.chooseNum(2L, Long.MaxValue).suchThat(_ > 1L),
      Gen.chooseNum(2L, Long.MaxValue).suchThat(_ > 1L),
      proveDlogGen
    ) {
      case (tokenId, tokenPrice, dexFeePerToken, proveDlogPk) =>
        val params =
          DexSellerContractParameters(proveDlogPk, tokenId, tokenPrice, dexFeePerToken)
        // println(params)
        val ergoTree = DexLimitOrderContracts.sellerContractInstance(params).ergoTree
        // println(ergoTree.constants.zipWithIndex)
        DexLimitOrderContracts
          .parseSellerContractParameters(ergoTree)
          .get shouldEqual params

        DexLimitOrderContracts.sellerContractErgoTreeTemplate shouldEqual ergoTree.template
    }
  }
}
