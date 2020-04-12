package org.ergoplatform.contracts

import stainless.annotation.ignore

import org.ergoplatform.compiler.ErgoContract
import sigmastate.Values.ErgoTree
import org.ergoplatform.compiler.ErgoScalaCompiler

final case class DexBuyerContractParameters(
  buyerPk: special.sigma.SigmaProp,
  tokenId: special.collection.Coll[Byte],
  tokenPrice: Long,
  dexFeePerToken: Long
)

final case class DexSellerContractParameters(
  sellerPk: special.sigma.SigmaProp,
  tokenId: special.collection.Coll[Byte],
  tokenPrice: Long,
  dexFeePerToken: Long
)

@ignore
sealed abstract class DexPartialFilling {}

@ignore
private object DexPartialFillingErgoScript {

  def buyerContract(params: DexBuyerContractParameters): ErgoContract = {
    val buyerContractEnv = Map("buyerPk" -> params.buyerPk, "tokenId" -> params.tokenId)

    val buyerScript = s"""buyerPk || {

      val tokenPrice = ${params.tokenPrice}
      val dexFeePerToken = ${params.dexFeePerToken}

      val returnBox = OUTPUTS.filter { (b: Box) => 
        b.R4[Coll[Byte]].isDefined && b.R4[Coll[Byte]].get == SELF.id && b.propositionBytes == buyerPk.propBytes
      }(0)

      val returnTokenData = returnBox.tokens(0)
      val returnTokenId = returnTokenData._1
      val returnTokenAmount = returnTokenData._2
      val maxReturnTokenErgValue = returnTokenAmount * tokenPrice
      val totalReturnErgValue = maxReturnTokenErgValue + returnBox.value
      val expectedDexFee = dexFeePerToken * returnTokenAmount
      
      val foundNewOrderBoxes = OUTPUTS.filter { (b: Box) => 
        b.R4[Coll[Byte]].isDefined && b.R4[Coll[Byte]].get == SELF.id && b.propositionBytes == SELF.propositionBytes
      }

      val coinsSecured = (SELF.value - expectedDexFee) == maxReturnTokenErgValue || {
        foundNewOrderBoxes.size == 1 && foundNewOrderBoxes(0).value >= (SELF.value - totalReturnErgValue - expectedDexFee)
      }

      val tokenIdIsCorrect = returnTokenId == tokenId
    
      allOf(Coll(
          tokenIdIsCorrect,
          returnTokenAmount >= 1,
          coinsSecured
      ))
    }
      """.stripMargin

    ErgoScalaCompiler.contract(buyerContractEnv, buyerScript)
  }

  def sellerContract(params: DexSellerContractParameters): ErgoContract = {

    val sellerContractEnv =
      Map("sellerPk" -> params.sellerPk, "tokenId" -> params.tokenId)

    val sellerScript = s""" sellerPk || {
      val tokenPrice = ${params.tokenPrice}
      val dexFeePerToken = ${params.dexFeePerToken}

      val selfTokenAmount = SELF.tokens(0)._2

      val returnBox = OUTPUTS.filter { (b: Box) =>
        b.R4[Coll[Byte]].isDefined && b.R4[Coll[Byte]].get == SELF.id && b.propositionBytes == sellerPk.propBytes
      }(0)

      val foundNewOrderBoxes = OUTPUTS.filter { (b: Box) =>
        b.R4[Coll[Byte]].isDefined && b.R4[Coll[Byte]].get == SELF.id && b.propositionBytes == SELF.propositionBytes
      }

      (returnBox.value == selfTokenAmount * tokenPrice) || {
        foundNewOrderBoxes.size == 1 && {
          val newOrderBox = foundNewOrderBoxes(0)
          val newOrderTokenData = newOrderBox.tokens(0)
          val newOrderTokenAmount = newOrderTokenData._2
          val soldTokenAmount = selfTokenAmount - newOrderTokenAmount
          val minSoldTokenErgValue = soldTokenAmount * tokenPrice
          val expectedDexFee = dexFeePerToken * soldTokenAmount

          val newOrderTokenId = newOrderTokenData._1
          val tokenIdIsCorrect = newOrderTokenId == tokenId

          tokenIdIsCorrect && soldTokenAmount >= 1 && newOrderBox.value >= (SELF.value - minSoldTokenErgValue - expectedDexFee)
        }
      }

      }""".stripMargin

    ErgoScalaCompiler.contract(sellerContractEnv, sellerScript)
  }

}

@ignore
object DexPartialFillingCompilation {

  def buyerContractInstance(parameters: DexBuyerContractParameters): ErgoContract =
    DexPartialFillingErgoScript.buyerContract(parameters)

  def sellerContractInstance(parameters: DexSellerContractParameters): ErgoContract =
    DexPartialFillingErgoScript.sellerContract(parameters)

  def parseBuyerContractParameters(
    ergoTree: ErgoTree
  ): Option[DexBuyerContractParameters] = ???

  def parseSellerContractParameters(
    ergoTree: ErgoTree
  ): Option[DexSellerContractParameters] = ???

}
