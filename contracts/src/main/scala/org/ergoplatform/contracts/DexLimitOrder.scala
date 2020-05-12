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
    val buyerContractEnv =
      Map(
        "buyerPk"        -> CSigmaProp(params.buyerPk),
        "tokenId"        -> params.tokenId.toColl,
        "tokenPrice"     -> params.tokenPrice,
        "dexFeePerToken" -> params.dexFeePerToken
      )

    val buyerScript = s"""buyerPk || {

      val spendingSellOrders = INPUTS.filter { (b: Box) => 
        b.R4[Coll[Byte]].isDefined && b.R5[Long].isDefined && {
          val sellOrderTokenId = b.R4[Coll[Byte]].get
          sellOrderTokenId == tokenId && {
            b.tokens.size == 1 && b.tokens(0)._1 == tokenId
          }
        }
      }

      val returnBoxes = OUTPUTS.filter { (b: Box) => 
        b.R4[Coll[Byte]].isDefined && b.R4[Coll[Byte]].get == SELF.id && b.propositionBytes == buyerPk.propBytes
      }

      val boxesAreSortedByTokenPrice = { (boxes: Coll[Box]) => 
        boxes.fold((0L, true), { (t: (Long, Boolean), box: Box) => 
          val prevBoxTokenPrice = t._1
          val isSorted = t._2
          val boxTokenPrice = box.R5[Long].getOrElse(0L)
          (boxTokenPrice, isSorted && boxTokenPrice >= prevBoxTokenPrice)
        })._2
      }

      returnBoxes.size == 1 && spendingSellOrders.size > 0 && boxesAreSortedByTokenPrice(spendingSellOrders) && {
        val returnBox = returnBoxes(0)
        val returnTokenAmount = if (returnBox.tokens.size == 1) returnBox.tokens(0)._2 else 0L
        
        val expectedDexFee = dexFeePerToken * returnTokenAmount
        
        val foundNewOrderBoxes = OUTPUTS.filter { (b: Box) => 
          val contractParametersAreCorrect = b.R4[Coll[Byte]].get == tokenId && b.R5[Long].get == tokenPrice
          b.R7[Coll[Byte]].isDefined && b.R7[Coll[Byte]].get == SELF.id && b.propositionBytes == SELF.propositionBytes
        }

        val fullSpread = {
          spendingSellOrders.fold((returnTokenAmount, 0L), { (t: (Long, Long), sellOrder: Box) => 
            val returnTokensLeft = t._1
            val accumulatedFullSpread = t._2
            val sellOrderTokenPrice = sellOrder.R5[Long].get
            val sellOrderTokenAmount = sellOrder.tokens(0)._2
            if (sellOrder.creationInfo._1 >= SELF.creationInfo._1 && sellOrderTokenPrice <= tokenPrice) {
              // spread is ours
              val spreadPerToken = tokenPrice - sellOrderTokenPrice
              val tokenAmount = min(returnTokensLeft, sellOrderTokenAmount)
              val sellOrderSpread = spreadPerToken * tokenAmount
              (returnTokensLeft - tokenAmount, accumulatedFullSpread + sellOrderSpread)
            }
            else {
              // spread is not ours
              (returnTokensLeft - min(returnTokensLeft, sellOrderTokenAmount), accumulatedFullSpread)
            }
          })._2
        }

        val totalMatching = (SELF.value - expectedDexFee) == returnTokenAmount * tokenPrice && 
          returnBox.value >= fullSpread
        val partialMatching = {
          foundNewOrderBoxes.size == 1 && 
            foundNewOrderBoxes(0).value == (SELF.value - returnTokenAmount * tokenPrice - expectedDexFee) &&
            returnBox.value >= fullSpread
        }

        val coinsSecured = partialMatching || totalMatching

        val tokenIdIsCorrect = returnBox.tokens.getOrElse(0, (Coll[Byte](), 0L))._1 == tokenId
        
        allOf(Coll(
            tokenIdIsCorrect,
            returnTokenAmount >= 1,
            coinsSecured
        ))
      }
    }
      """.stripMargin

    ErgoScalaCompiler.contract(buyerContractEnv, buyerScript)
  }

  def sellerContract(params: DexSellerContractParameters): ErgoContract = {

    val sellerContractEnv =
      Map(
        "sellerPk"       -> CSigmaProp(params.sellerPk),
        "tokenId"        -> params.tokenId.toColl,
        "tokenPrice"     -> params.tokenPrice,
        "dexFeePerToken" -> params.dexFeePerToken
      )

    val sellerScript = s""" sellerPk || {

      val selfTokenAmount = SELF.tokens(0)._2

      val returnBoxes = OUTPUTS.filter { (b: Box) => 
        b.R4[Coll[Byte]].isDefined && b.R4[Coll[Byte]].get == SELF.id && b.propositionBytes == sellerPk.propBytes
      }

      val boxesAreSortedByTokenPrice = { (boxes: Coll[Box]) => 
        boxes.fold((0L, true), { (t: (Long, Boolean), box: Box) => 
          val prevBoxTokenPrice = t._1
          val isSorted = t._2
          val boxTokenPrice = box.R5[Long].getOrElse(0L)
          (boxTokenPrice, isSorted && boxTokenPrice >= prevBoxTokenPrice)
        })._2
      }

      val spendingBuyOrders = INPUTS.filter { (b: Box) => 
        b.R4[Coll[Byte]].isDefined && b.R5[Long].isDefined && b.R6[Long].isDefined && {
          val buyOrderTokenId = b.R4[Coll[Byte]].get
          buyOrderTokenId == tokenId && b.tokens.size == 0 
        }
      }

      returnBoxes.size == 1 && spendingBuyOrders.size > 0 && boxesAreSortedByTokenPrice(spendingBuyOrders) && {
        val returnBox = returnBoxes(0)

        val foundNewOrderBoxes = OUTPUTS.filter { (b: Box) => 
          val contractParametersAreCorrect = b.R4[Coll[Byte]].get == tokenId && b.R5[Long].get == tokenPrice
          val contractIsTheSame = b.propositionBytes == SELF.propositionBytes
          b.R7[Coll[Byte]].isDefined && b.R7[Coll[Byte]].get == SELF.id && contractIsTheSame
        }

        val fullSpread = { (tokenAmount: Long) =>
          spendingBuyOrders.fold((tokenAmount, 0L), { (t: (Long, Long), buyOrder: Box) => 
            val returnTokensLeft = t._1
            val accumulatedFullSpread = t._2
            val buyOrderTokenPrice = buyOrder.R5[Long].get
            val buyOrderDexFeePerToken = buyOrder.R6[Long].get
            val buyOrderTokenAmount = buyOrder.value / (buyOrderTokenPrice + buyOrderDexFeePerToken)
            if (buyOrder.creationInfo._1 >= SELF.creationInfo._1 && buyOrderTokenPrice <= tokenPrice) {
              // spread is ours
              val spreadPerToken = tokenPrice - buyOrderTokenPrice
              val tokenAmountLeft = min(returnTokensLeft, buyOrderTokenAmount)
              val buyOrderSpread = spreadPerToken * tokenAmountLeft
              (returnTokensLeft - tokenAmountLeft, accumulatedFullSpread + buyOrderSpread)
            }
            else {
              // spread is not ours
              (returnTokensLeft - min(returnTokensLeft, buyOrderTokenAmount), accumulatedFullSpread)
            }
          })._2
        }

        val totalMatching = (returnBox.value == selfTokenAmount * tokenPrice + fullSpread(selfTokenAmount))

        val partialMatching = {
          foundNewOrderBoxes.size == 1 && {
            val newOrderBox = foundNewOrderBoxes(0)
            val newOrderTokenData = newOrderBox.tokens(0)
            val newOrderTokenAmount = newOrderTokenData._2
            val soldTokenAmount = selfTokenAmount - newOrderTokenAmount
            val minSoldTokenErgValue = soldTokenAmount * tokenPrice
            val expectedDexFee = dexFeePerToken * soldTokenAmount

            val newOrderTokenId = newOrderTokenData._1
            val tokenIdIsCorrect = newOrderTokenId == tokenId

            val newOrderValueIsCorrect = newOrderBox.value == (SELF.value - expectedDexFee)
            val returnBoxValueIsCorrect = returnBox.value == soldTokenAmount * tokenPrice + fullSpread(soldTokenAmount)
            tokenIdIsCorrect && soldTokenAmount >= 1 && newOrderValueIsCorrect && returnBoxValueIsCorrect
          }
        }

        (totalMatching || partialMatching) 
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
