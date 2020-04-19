package org.ergoplatform.contracts

import stainless.annotation.ignore

import org.ergoplatform.compiler._
import org.ergoplatform.sigma.verified._
import stainless.annotation.ignore
import stainless.lang._

sealed abstract class DexPartialFilling extends SigmaContract {

  def buyerContract(
    ctx: Context,
    buyerPk: SigmaProp,
    tokenId: Coll[Byte],
    tokenPrice: Long,
    dexFeePerToken: Long
  ): SigmaProp = {
    // both value cannot be 1, otherwise compiler reduces b = a * 1 to b = a,
    // eliminating them from the expression (and from ErgoTree.constants)
    // require(dexFeePerToken > 1)
    // require(tokenPrice > 1)

    import ctx._

    buyerPk || {

      val returnBox = OUTPUTS.filter { (b: Box) =>
        b.R4[Coll[Byte]].isDefined && b
          .R4[Coll[Byte]]
          .get == SELF.id && b.propositionBytes == buyerPk.propBytes
      }(0)

      val returnTokenData        = returnBox.tokens(0)
      val returnTokenId          = returnTokenData._1
      val returnTokenAmount      = returnTokenData._2
      val maxReturnTokenErgValue = returnTokenAmount * tokenPrice
      val totalReturnErgValue    = maxReturnTokenErgValue + returnBox.value
      val expectedDexFee         = dexFeePerToken * returnTokenAmount

      val foundNewOrderBoxes = OUTPUTS.filter { (b: Box) =>
        b.R4[Coll[Byte]].isDefined && b
          .R4[Coll[Byte]]
          .get == SELF.id && b.propositionBytes == SELF.propositionBytes
      }

      val coinsSecured = (SELF.value - expectedDexFee) == maxReturnTokenErgValue || {
        foundNewOrderBoxes.size == 1 && foundNewOrderBoxes(0).value >= (SELF.value - totalReturnErgValue - expectedDexFee)
      }

      val tokenIdIsCorrect = returnTokenId == tokenId

      tokenIdIsCorrect && returnTokenAmount >= 1 && coinsSecured
    }

  }

}

case object DexPartialFillingVerification extends DexPartialFilling {

  // private def conditionCorrectClaimableTokenAmountAgainstBuyerBox(
  //   ctx: Context,
  //   tokenId: Coll[Byte],
  //   tokenAmount: Long,
  //   pkA: SigmaProp
  // ): Boolean = {
  //   import ctx._
  //   OUTPUTS.nonEmpty &&
  //   OUTPUTS(0).tokens.nonEmpty &&
  //   OUTPUTS(0).R4[Coll[Byte]].isDefined &&
  //   OUTPUTS(0).tokens(0)._1 == tokenId &&
  //   OUTPUTS(0).tokens(0)._2 >= tokenAmount &&
  //   OUTPUTS(0).propositionBytes == pkA.propBytes &&
  //   OUTPUTS(0).R4[Coll[Byte]].get == SELF.id
  // }

  // def proveBuyerCanClaim(
  //   ctx: Context,
  //   tokenId: Coll[Byte],
  //   tokenAmount: Long,
  //   pkA: SigmaProp
  // ): Boolean = {
  //   require(pkA.isValid)
  //   buyer(ctx, tokenId, tokenAmount, pkA).isValid
  // } holds

  // def proveSpendableTokensAgainstThisOrderAnyTime(
  //   ctx: Context,
  //   tokenId: Coll[Byte],
  //   tokenAmount: Long,
  //   pkA: SigmaProp
  // ): Boolean = {
  //   require(
  //     conditionCorrectClaimableTokenAmountAgainstBuyerBox(ctx, tokenId, tokenAmount, pkA)
  //   )
  //   buyer(ctx, tokenId, tokenAmount, pkA).isValid
  // } holds
}
