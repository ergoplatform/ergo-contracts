package org.ergoplatform.contracts

import org.ergoplatform.compiler._
import org.ergoplatform.sigma.verified._
import stainless.annotation.ignore
import stainless.lang._

import scala.language.{implicitConversions, postfixOps}

sealed abstract class AssetsAtomicExchange extends SigmaContract {

  /** DEX contract for buy order
    * Bid order to buy a token specified by token id and amount for a price specified in a box value that
    * will be protected by this contract. This box can be spend in two ways:
    * 1. Buyer wants to cancel the order and return the coins (buyerPk is enough).
    * 2. With a box holding a specified amount of token id, with this buy order box id in it's R4,
    * and claimable by buyerPk.
    * Used in ErgoTool "dex:BuyOrder" command.
    * @param tokenId token id to buy
    * @param tokenAmount token amount to buy
    * @param buyerPk public key for the buyer
    * @return compiled contract
    */
  def buyer(
    ctx: Context,
    tokenId: Coll[Byte],
    tokenAmount: Long,
    buyerPk: SigmaProp
  ): SigmaProp = {
    import ctx._
    buyerPk || {
      (OUTPUTS.nonEmpty && OUTPUTS(0).R4[Coll[Byte]].isDefined) && {
        val tokens = OUTPUTS(0).tokens
        val tokenDataCorrect = tokens.nonEmpty &&
          tokens(0)._1 == tokenId &&
          tokens(0)._2 >= tokenAmount

        val knownId = OUTPUTS(0).R4[Coll[Byte]].get == SELF.id
        tokenDataCorrect &&
        OUTPUTS(0).propositionBytes == buyerPk.propBytes &&
        knownId
      }
    }
  }

  /** DEX contract for sell order
    * Ask order to sell a token for a specified price (ergAmount).
    * Token is specified by token id and amount in a box that will be protected by this contract
    * This box can be spend in two ways:
    * 1. Seller wants to cancel the order and return the tokens (sellerPk is enough).
    * 2. Buyer creates a box value with a specified ergAmount,
    * puts this sell order box id in R4, and make it claimable by sellerPk.
    * Used in ErgoTool "dex:SellOrder" command.
    * @param ergAmount nanoERG amount seller wants to receive for the tokens
    * @param sellerPk public key of the seller
    * @return compiled contract
    */
  def seller(ctx: Context, ergAmount: Long, sellerPk: SigmaProp): SigmaProp = {
    import ctx._
    sellerPk || (
      OUTPUTS.size > 1 &&
      OUTPUTS(1).R4[Coll[Byte]].isDefined
    ) && {
      val knownBoxId = OUTPUTS(1).R4[Coll[Byte]].get == SELF.id
      OUTPUTS(1).value >= ergAmount &&
      knownBoxId &&
      OUTPUTS(1).propositionBytes == sellerPk.propBytes
    }
  }
}

case object AssetsAtomicExchangeBuyerVerification extends AssetsAtomicExchange {

  private def conditionCorrectClaimableTokenAmountAgainstBuyerBox(
    ctx: Context,
    tokenId: Coll[Byte],
    tokenAmount: Long,
    pkA: SigmaProp
  ): Boolean = {
    import ctx._
    OUTPUTS.nonEmpty &&
    OUTPUTS(0).tokens.nonEmpty &&
    OUTPUTS(0).R4[Coll[Byte]].isDefined &&
    OUTPUTS(0).tokens(0)._1 == tokenId &&
    OUTPUTS(0).tokens(0)._2 >= tokenAmount &&
    OUTPUTS(0).propositionBytes == pkA.propBytes &&
    OUTPUTS(0).R4[Coll[Byte]].get == SELF.id
  }

  def proveBuyerCanClaim(
    ctx: Context,
    tokenId: Coll[Byte],
    tokenAmount: Long,
    pkA: SigmaProp
  ): Boolean = {
    import ctx._
    require(pkA.isValid)
    buyer(ctx, tokenId, tokenAmount, pkA).isValid
  } holds

  def proveSpendableTokensAgainstThisOrderAnyTime(
    ctx: Context,
    tokenId: Coll[Byte],
    tokenAmount: Long,
    pkA: SigmaProp
  ): Boolean = {
    import ctx._
    require(
      conditionCorrectClaimableTokenAmountAgainstBuyerBox(ctx, tokenId, tokenAmount, pkA)
    )
    buyer(ctx, tokenId, tokenAmount, pkA).isValid
  } holds
}

case object AssetsAtomicExchangeSellerVerification extends AssetsAtomicExchange {

  private def conditionClaimableWithCorrectErgAmount(
    ctx: Context,
    ergAmount: Long,
    pkB: SigmaProp
  ): Boolean = {
    import ctx._
    OUTPUTS.size > 1 &&
    OUTPUTS(1).R4[Coll[Byte]].isDefined &&
    OUTPUTS(1).value >= ergAmount &&
    OUTPUTS(1).R4[Coll[Byte]].get == SELF.id &&
    OUTPUTS(1).propositionBytes == pkB.propBytes
  }

  def proveSellerCanClaim(ctx: Context, ergAmount: Long, pkB: SigmaProp): Boolean = {
    import ctx._
    require(pkB.isValid)
    seller(ctx, ergAmount, pkB).isValid
  } holds

  def proveSpendableErgAgainstThisOrder(
    ctx: Context,
    ergAmount: Long,
    pkB: SigmaProp
  ): Boolean = {
    import ctx._
    require(conditionClaimableWithCorrectErgAmount(ctx, ergAmount, pkB))
    seller(ctx, ergAmount, pkB).isValid
  } holds
}

@ignore
object AssetsAtomicExchangeCompilation extends AssetsAtomicExchange {

  def buyerContractInstance(
    tokenId: special.collection.Coll[Byte],
    tokenAmount: Long,
    pkA: special.sigma.SigmaProp
  ): ErgoContract = {
    import org.ergoplatform.sigma.verified.VerifiedTypeConverters._
    ErgoScalaCompiler.contractVerified { context: Context =>
      buyer(context, tokenId, tokenAmount, pkA)
    }
  }

  def sellerContractInstance(
    ergAmount: Long,
    pkB: special.sigma.SigmaProp
  ): ErgoContract = {
    import org.ergoplatform.sigma.verified.VerifiedTypeConverters._
    ErgoScalaCompiler.contractVerified { context: Context =>
      seller(context, ergAmount, pkB)
    }
  }

}
