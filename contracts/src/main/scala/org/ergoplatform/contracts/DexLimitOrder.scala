package org.ergoplatform.contracts

import org.ergoplatform.compiler.ErgoContract
import sigmastate.Values.ErgoTree
import org.ergoplatform.compiler.ErgoScalaCompiler
import sigmastate.SLong
import sigmastate.Values
import sigmastate.interpreter.CryptoConstants
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval.CSigmaProp
import sigmastate.eval.Extensions.ArrayOps
import org.ergoplatform.ErgoBox
import sigmastate.Values.ByteArrayConstant
import sigmastate.Values.SigmaPropConstant
import sigmastate.basics.DLogProtocol.ProveDlogProp

final case class DexBuyerContractParameters(
  buyerPk: ProveDlog,
  tokenId: Array[Byte],
  tokenPrice: Long,
  dexFeePerToken: Long
)

final case class DexSellerContractParameters(
  sellerPk: ProveDlog,
  tokenId: Array[Byte],
  tokenPrice: Long,
  dexFeePerToken: Long
)

private object DexLimitOrderErgoScript {

  def buyerContract(params: DexBuyerContractParameters): ErgoContract = {
    // both value cannot be 1, otherwise compiler reduces b = a * 1 to b = a,
    // eliminating them from the expression (and from ErgoTree.constants)
    require(params.dexFeePerToken > 1, "dexFeePerToken should be > 1")
    require(params.tokenPrice > 1, "tokenPrice should be > 1")
    val buyerContractEnv =
      Map(
        "buyerPk"        -> CSigmaProp(params.buyerPk),
        "tokenId"        -> params.tokenId.toColl,
        "tokenPrice"     -> params.tokenPrice,
        "dexFeePerToken" -> params.dexFeePerToken
      )

    val buyerScript = s"""buyerPk || {
                          
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
    // both value cannot be 1, otherwise compiler reduces b = a * 1 to b = a,
    // eliminating them from the expression (and from ErgoTree.constants)
    require(params.dexFeePerToken > 1, "dexFeePerToken should be > 1")
    require(params.tokenPrice > 1, "tokenPrice should be > 1")

    val sellerContractEnv =
      Map(
        "sellerPk"       -> CSigmaProp(params.sellerPk),
        "tokenId"        -> params.tokenId.toColl,
        "tokenPrice"     -> params.tokenPrice,
        "dexFeePerToken" -> params.dexFeePerToken
      )

    val sellerScript = s""" sellerPk || {

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

object DexLimitOrderContracts {

  def buyerContractInstance(parameters: DexBuyerContractParameters): ErgoContract =
    DexLimitOrderErgoScript.buyerContract(parameters)

  def sellerContractInstance(parameters: DexSellerContractParameters): ErgoContract =
    DexLimitOrderErgoScript.sellerContract(parameters)

  def parseBuyerContractParameters(
    ergoTree: ErgoTree
  ): Option[DexBuyerContractParameters] =
    for {
      pk <- ergoTree.constants.headOption.collect {
             case SigmaPropConstant(ProveDlogProp(v)) => v
           }
      tokenId <- ergoTree.constants.lift(5).collect {
                  case ByteArrayConstant(coll) => coll.toArray
                }
      tokenPrice <- ergoTree.constants.lift(4).collect {
                     case Values.ConstantNode(value, SLong) => value.asInstanceOf[Long]
                   }
      dexFeePerToken <- ergoTree.constants.lift(3).collect {
                         case Values.ConstantNode(value, SLong) =>
                           value.asInstanceOf[Long]
                       }
    } yield DexBuyerContractParameters(pk, tokenId, tokenPrice, dexFeePerToken)

  def parseSellerContractParameters(
    ergoTree: ErgoTree
  ): Option[DexSellerContractParameters] =
    for {
      pk <- ergoTree.constants.headOption.collect {
             case SigmaPropConstant(ProveDlogProp(v)) => v
           }
      tokenId <- ergoTree.constants.lift(7).collect {
                  case ByteArrayConstant(coll) => coll.toArray
                }
      tokenPrice <- ergoTree.constants.lift(3).collect {
                     case Values.ConstantNode(value, SLong) => value.asInstanceOf[Long]
                   }
      dexFeePerToken <- ergoTree.constants.lift(10).collect {
                         case Values.ConstantNode(value, SLong) =>
                           value.asInstanceOf[Long]
                       }
    } yield DexSellerContractParameters(pk, tokenId, tokenPrice, dexFeePerToken)

  lazy val buyerContractErgoTreeTemplate: Array[Byte] = {
    val tokenId = Array.fill(ErgoBox.TokenId.size)(0.toByte)
    val pk      = ProveDlog(CryptoConstants.dlogGroup.createRandomElement())
    val params  = DexBuyerContractParameters(pk, tokenId, 2L, 2L)
    val c       = DexLimitOrderErgoScript.buyerContract(params)
    c.ergoTree.template
  }

  lazy val sellerContractErgoTreeTemplate: Array[Byte] = {
    val tokenId = Array.fill(ErgoBox.TokenId.size)(0.toByte)
    val pk      = ProveDlog(CryptoConstants.dlogGroup.createRandomElement())
    val params  = DexSellerContractParameters(pk, tokenId, 2L, 2L)
    val c       = DexLimitOrderErgoScript.sellerContract(params)
    c.ergoTree.template
  }

}
