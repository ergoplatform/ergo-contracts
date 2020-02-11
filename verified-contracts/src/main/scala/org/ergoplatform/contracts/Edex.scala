package org.ergoplatform.contracts

import org.ergoplatform.sigma.verified._
import stainless.annotation.ignore

import scala.language.{implicitConversions, postfixOps}

@ignore
sealed abstract class Edex extends SigmaContract {

  def buyOrder(
    ctx: Context,
    tokenId: Coll[Byte],
    tokenAmount: Long,
    buyerPk: SigmaProp
  ): SigmaProp = {
    import ctx._
    buyerPk || {
      (
        OUTPUTS.nonEmpty &&
        OUTPUTS(0).R4[Coll[Byte]].isDefined &&
        OUTPUTS(0).tokens.nonEmpty
      ) && {
        val boxWithTokens     = OUTPUTS(0)
        val tokens            = boxWithTokens.tokens(0)
        val tokenIdIsCorrect  = tokens._1 == tokenId
        val givesEnoughTokens = tokens._2 >= tokenAmount
        val choosesMe         = boxWithTokens.R4[Coll[Byte]].get == SELF.id
        val iCanSpend         = boxWithTokens.propositionBytes == buyerPk.propBytes

        choosesMe && tokenIdIsCorrect && givesEnoughTokens && iCanSpend
      }
    }
  }

  def sellOrder(
    ctx: Context,
    tokenId: Coll[Byte],
    tokenPrice: Long,
    sellerPk: SigmaProp
  ): SigmaProp = {
    import ctx._
    sellerPk || {
      (
        OUTPUTS.size > 1 &&
        OUTPUTS(1).R4[Coll[Byte]].isDefined &&
        SELF.tokens.nonEmpty
      ) && {
        val boxWithCoins     = OUTPUTS(1)
        val tokenForSale     = SELF.tokens(0)
        val tokenIdIsCorrect = tokenForSale._1 == tokenId
        val choosesMe        = boxWithCoins.R4[Coll[Byte]].get == SELF.id

        val givesEnoughCoins = boxWithCoins.value >= (tokenForSale._2 * tokenPrice)

        val iCanSpend = boxWithCoins.propositionBytes == sellerPk.propBytes

        choosesMe && tokenIdIsCorrect && givesEnoughCoins && iCanSpend
      }
    }
  }

  // TODO: it's the same as sellOrder, only with a different output box index (add index parameter and reuse)
  def edexTokenSellOrder(
    ctx: Context,
    tokenId: Coll[Byte],
    tokenPrice: Long,
    dexPk: SigmaProp
  ): SigmaProp = {
    import ctx._
    dexPk || {
      (
        OUTPUTS.nonEmpty &&
        OUTPUTS(0).R4[Coll[Byte]].isDefined &&
        SELF.tokens.nonEmpty
      ) && {
        val boxWithCoins         = OUTPUTS(0)
        val tokenForSale         = SELF.tokens(0)
        val edexTokenIdIsCorrect = tokenForSale._1 == tokenId
        val choosesMe            = boxWithCoins.R4[Coll[Byte]].get == SELF.id

        val givesEnoughCoins = boxWithCoins.value >= (tokenForSale._2 * tokenPrice)

        val iCanSpend = boxWithCoins.propositionBytes == dexPk.propBytes

        choosesMe && edexTokenIdIsCorrect && givesEnoughCoins && iCanSpend
      }
    }
  }
}
