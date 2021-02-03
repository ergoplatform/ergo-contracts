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

/** Parameters for DEX limit order buyer contract
  * @param buyerPk buyer's PK (used for canceling the contract (spending the box).
  * @param tokenId token id buyer wants to buy
  * @param tokenPrice price per token in nanoERGs
  * @param dexFeePerToken DEX matcher's reward (per token, nanoERGs)
  * Keep in mind, that buyer box value should satisfy `box.value % (tokenPrice + dexFeePerToken) == 0` requirement.
  */
final case class DexBuyerContractParameters(
  buyerPk: ProveDlog,
  tokenId: Array[Byte],
  tokenPrice: Long,
  dexFeePerToken: Long
)

/** Parameters for DEX limit order seller contract
  * @param sellerPk seller's PK (used for canceling the contract (spending the box).
  * @param tokenId token id seller wants to sell
  * @param tokenPrice price per token in nanoERGs
  * @param dexFeePerToken DEX matcher's reward (per token, nanoERGs)
  */
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

      // counter(sell) orders that are matched against this order
      val spendingSellOrders = INPUTS.filter { (b: Box) => 
        b.R4[Coll[Byte]].isDefined && b.R5[Long].isDefined && {
          val sellOrderTokenId = b.R4[Coll[Byte]].get
          sellOrderTokenId == tokenId && {
            b.tokens.size == 1 && b.tokens(0)._1 == tokenId
          }
        }
      }

      // box with mine(bought) tokens
      // check that such box is only one in outputs is later in the code
      val returnBoxes = OUTPUTS.filter { (b: Box) => 
        val referencesMe = b.R4[Coll[Byte]].isDefined && b.R4[Coll[Byte]].get == SELF.id 
        val canSpend = b.propositionBytes == buyerPk.propBytes
        referencesMe && canSpend      
      }

      // check if this order should get the spread for a given counter order(height)
      val spreadIsMine = { (counterOrderBoxHeight: Int) => 
        // greater or equal since only a strict greater gives win in sell order contract
        // Denys: we have to decide who gets the spread if height is equal, without any reason I chose buy order
        counterOrderBoxHeight >= SELF.creationInfo._1 
      }

      // check that counter(sell) orders are sorted by spread in INPUTS
      // so that the bigger(top) spread will be "consumed" first
      val sellOrderBoxesAreSortedBySpread = { (boxes: Coll[Box]) => 
        boxes.size > 0 && {
          val alledgedlyTopSpread = if (spreadIsMine(boxes(0).creationInfo._1)) { 
            tokenPrice - boxes(0).R5[Long].getOrElse(0L)
          } else { 0L }
          boxes.fold((alledgedlyTopSpread, true), { (t: (Long, Boolean), box: Box) => 
            val prevSpread = t._1
            val isSorted = t._2
            val boxTokenPrice = box.R5[Long].getOrElse(0L)
            val boxTokenPriceIsCorrect = boxTokenPrice > 0 && boxTokenPrice <= tokenPrice
            val spread = if (spreadIsMine(box.creationInfo._1)) { 
              tokenPrice - boxTokenPrice 
            } else { 0L }
            (spread, isSorted && boxTokenPriceIsCorrect && spread <= prevSpread)
          })._2 
        }
      }

      returnBoxes.size == 1 && 
        spendingSellOrders.size > 0 && 
        sellOrderBoxesAreSortedBySpread(spendingSellOrders) && {

        val returnBox = returnBoxes(0)
        // token amount that are bought
        val returnTokenAmount = if (returnBox.tokens.size == 1) returnBox.tokens(0)._2 else 0L
        
        // DEX fee that we allow for matcher to take
        val expectedDexFee = dexFeePerToken * returnTokenAmount
        
        // in case of partial matching new buy order box should be created with funds that are not matched in this tx
        // check that there is only one such box is made later in the code
        val foundResidualOrderBoxes = OUTPUTS.filter { (b: Box) => 
          val tokenIdParamIsCorrect = b.R4[Coll[Byte]].isDefined && b.R4[Coll[Byte]].get == tokenId 
          val tokenPriceParamIsCorrect = b.R5[Long].isDefined && b.R5[Long].get == tokenPrice
          val dexFeePerTokenParamIsCorrect = b.R6[Long].isDefined && b.R6[Long].get == dexFeePerToken
          val contractParamsAreCorrect = tokenIdParamIsCorrect && 
            tokenPriceParamIsCorrect && dexFeePerTokenParamIsCorrect
          val referenceMe = b.R7[Coll[Byte]].isDefined && b.R7[Coll[Byte]].get == SELF.id 
          val guardedByTheSameContract = b.propositionBytes == SELF.propositionBytes
          contractParamsAreCorrect && referenceMe && guardedByTheSameContract
        }

        // aggregated spread we get from all counter(sell) orders
        val fullSpread = {
          spendingSellOrders.fold((returnTokenAmount, 0L), { (t: (Long, Long), sellOrder: Box) => 
            val returnTokensLeft = t._1
            val accumulatedFullSpread = t._2
            val sellOrderTokenPrice = sellOrder.R5[Long].get
            val sellOrderTokenAmount = sellOrder.tokens(0)._2
            val tokenAmountFromThisOrder = min(returnTokensLeft, sellOrderTokenAmount)
            if (spreadIsMine(sellOrder.creationInfo._1)) {
              // spread is ours
              val spreadPerToken = tokenPrice - sellOrderTokenPrice
              val sellOrderSpread = spreadPerToken * tokenAmountFromThisOrder
              (returnTokensLeft - tokenAmountFromThisOrder, accumulatedFullSpread + sellOrderSpread)
            }
            else {
              // spread is not ours
              (returnTokensLeft - tokenAmountFromThisOrder, accumulatedFullSpread)
            }
          })._2
        }

        // ERGs paid for the bought tokens
        val returnTokenValue = returnTokenAmount * tokenPrice
        // branch for total matching (all ERGs are spent and correct amount of tokens is bought)
        val totalMatching = (SELF.value - expectedDexFee) == returnTokenValue && 
          returnBox.value >= fullSpread
        // branch for partial matching, e.g. besides bought tokens we demand a new buy order with ERGs for 
        // non-matched part of this order
        val partialMatching = {
          val correctResidualOrderBoxValue = (SELF.value - returnTokenValue - expectedDexFee)
          foundResidualOrderBoxes.size == 1 && 
            foundResidualOrderBoxes(0).value == correctResidualOrderBoxValue && 
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

      // box with ERGs(mine) for sold tokens
      // check that such box is only one in outputs is later in the code
      val returnBoxes = OUTPUTS.filter { (b: Box) => 
        val referencesMe = b.R4[Coll[Byte]].isDefined && b.R4[Coll[Byte]].get == SELF.id
        val canSpend = b.propositionBytes == sellerPk.propBytes
        referencesMe && canSpend      
      }

      // check if this order should get the spread for a given counter order(height)
      val spreadIsMine = { (counterOrderBoxHeight: Int) => 
        // strictly greater since equality gives win in buy order contract
        // Denys: we have to decide who gets the spread if height is equal, without any reason I chose buy order
        counterOrderBoxHeight > SELF.creationInfo._1 
      }

      // check that counter(buy) orders are sorted by spread in INPUTS
      // so that the bigger(top) spread will be "consumed" first
      val buyOrderBoxesAreSortedBySpread = { (boxes: Coll[Box]) => 
        boxes.size > 0 && {
          val alledgedlyTopSpread = if (spreadIsMine(boxes(0).creationInfo._1)) { 
            boxes(0).R5[Long].getOrElse(0L) - tokenPrice 
          } else { 0L }
          boxes.fold((alledgedlyTopSpread, true), { (t: (Long, Boolean), box: Box) => 
            val prevSpread = t._1
            val isSorted = t._2
            val boxTokenPrice = box.R5[Long].getOrElse(0L)
            // although buy order's DEX fee is not used here, we check if its positive as a part of sanity check
            val boxDexFeePerToken = box.R6[Long].getOrElse(0L)
            val spread = if (spreadIsMine(box.creationInfo._1)) { boxTokenPrice - tokenPrice } else { 0L }
            (spread, isSorted && boxTokenPrice >= tokenPrice && boxDexFeePerToken > 0L && spread <= prevSpread)
          })._2 
        }
      }

      // counter(buy) orders that are matched against this order
      val spendingBuyOrders = INPUTS.filter { (b: Box) => 
        b.R4[Coll[Byte]].isDefined && b.R5[Long].isDefined && b.R6[Long].isDefined && {
          val buyOrderTokenId = b.R4[Coll[Byte]].get
          buyOrderTokenId == tokenId && b.tokens.size == 0 
        }
      }

      returnBoxes.size == 1 && 
        spendingBuyOrders.size > 0 && 
        buyOrderBoxesAreSortedBySpread(spendingBuyOrders) && {

        val returnBox = returnBoxes(0)

        // in case of partial matching new sell order box should be created with tokens that are not matched in this tx
        // check that there is only one such box is made later in the code
        val foundResidualOrderBoxes = OUTPUTS.filter { (b: Box) => 
          val tokenIdParamIsCorrect = b.R4[Coll[Byte]].isDefined && b.R4[Coll[Byte]].get == tokenId 
          val tokenPriceParamIsCorrect = b.R5[Long].isDefined && b.R5[Long].get == tokenPrice
          val dexFeePerTokenParamIsCorrect = b.R6[Long].isDefined && b.R6[Long].get == dexFeePerToken
          val contractParamsAreCorrect = tokenIdParamIsCorrect && 
            tokenPriceParamIsCorrect && 
            dexFeePerTokenParamIsCorrect
          val referenceMe = b.R7[Coll[Byte]].isDefined && b.R7[Coll[Byte]].get == SELF.id 
          val guardedByTheSameContract = b.propositionBytes == SELF.propositionBytes
          contractParamsAreCorrect && referenceMe && guardedByTheSameContract
        }

        // aggregated spread we get from all counter(buy) orders
        val fullSpread = { (tokenAmount: Long) =>
          spendingBuyOrders.fold((tokenAmount, 0L), { (t: (Long, Long), buyOrder: Box) => 
            val returnTokensLeft = t._1
            val accumulatedFullSpread = t._2
            val buyOrderTokenPrice = buyOrder.R5[Long].get
            val buyOrderDexFeePerToken = buyOrder.R6[Long].get
            val buyOrderTokenAmountCapacity = buyOrder.value / (buyOrderTokenPrice + buyOrderDexFeePerToken)
            val tokenAmountInThisOrder = min(returnTokensLeft, buyOrderTokenAmountCapacity)
            if (spreadIsMine(buyOrder.creationInfo._1)) {
              // spread is ours
              val spreadPerToken = buyOrderTokenPrice - tokenPrice
              val buyOrderSpread = spreadPerToken * tokenAmountInThisOrder
              (returnTokensLeft - tokenAmountInThisOrder, accumulatedFullSpread + buyOrderSpread)
            }
            else {
              // spread is not ours
              (returnTokensLeft - tokenAmountInThisOrder, accumulatedFullSpread)
            }
          })._2
        }

        // branch for total matching (all tokens are sold and full amount ERGs received)
        val totalMatching = (returnBox.value == selfTokenAmount * tokenPrice + fullSpread(selfTokenAmount))

        // branch for partial matching, e.g. besides received ERGs we demand a new sell order with tokens for 
        // non-matched part of this order
        val partialMatching = {
          foundResidualOrderBoxes.size == 1 && {
            val residualOrderBox = foundResidualOrderBoxes(0)
            val residualOrderTokenData = residualOrderBox.tokens(0)
            val residualOrderTokenAmount = residualOrderTokenData._2
            val soldTokenAmount = selfTokenAmount - residualOrderTokenAmount
            val soldTokenErgValue = soldTokenAmount * tokenPrice
            val expectedDexFee = dexFeePerToken * soldTokenAmount

            val residualOrderTokenId = residualOrderTokenData._1
            val tokenIdIsCorrect = residualOrderTokenId == tokenId

            val residualOrderValueIsCorrect = residualOrderBox.value == (SELF.value - expectedDexFee)
            val returnBoxValueIsCorrect = returnBox.value == soldTokenErgValue + fullSpread(soldTokenAmount)
            tokenIdIsCorrect && 
              soldTokenAmount >= 1 && 
              residualOrderValueIsCorrect && 
              returnBoxValueIsCorrect
          }
        }

        (totalMatching || partialMatching) 
      }

      }""".stripMargin

    ErgoScalaCompiler.contract(sellerContractEnv, sellerScript)
  }

}

object DexLimitOrderContracts {

  /** Compiles buyer's DEX limit order contract to ErgoTree
    * Parameters:
    * @param buyerPk buyer's PK (used for canceling the contract (spending the box).
    * @param tokenId token id buyer wants to buy
    * @param tokenPrice price per token in nanoERGs
    * @param dexFeePerToken DEX matcher's reward (per token, nanoERGs)
    * The value of this buy order box is expected to be = tokenAmount * (tokenPrice + dexFeePerToken)
    *
    * Requirements for inputs.
    * Matched sell counter order boxes should have the following properties:
    * Registers:
    * R4[Coll[Byte]] - token id of the sell order;
    * R5[Long] - token price of the sell order;
    * Other:
    * - box.tokens should contain only the above mentioned token id;
    * Matched sell counter orders should be ranged in the inputs
    * by the "won" spread (spread = sellOrderTokenPrice - buyOrderTokenPrice if it's won
    * or spread = 0 if it's lost).
    *
    * Requirements for outputs.
    * Box with bought tokens(return box).
    * There should be only one box with bought tokens with the following properties:
    * - box.tokens.size == 1 and contain the bought token amount(returnTokenAmount);
    * - R4[Coll[Byte]] == this order box id;
    * - should be guarded only by buyerPk only;
    * - box value should contain the spread (if there is any).
    * The spread is calculated by going through counter sell orders and taking the price difference
    * if this order is older or the same age as counter sell order (comparing creation height).
    *
    * If match is partial the new(residual) buy order box have to be in outputs.
    * There is can be only one such box in the outputs.
    * It should have the following registers set:
    * - R4[Coll[Byte]] = this buy order token id
    * - R5[Long] = this buy order `tokenPrice`
    * - R6[Long] = this buy order `dexFeePerToken`
    * - R7[Coll[Byte]] = this buy order box id
    * The residual box value should be (SELF.value - returnTokenAmount * tokenPrice - expectedDexFee)
    */
  def buyerContractInstance(parameters: DexBuyerContractParameters): ErgoContract =
    DexLimitOrderErgoScript.buyerContract(parameters)

  /** Compiles seller's DEX limit order contract to ErgoTree
    * Parameters:
    * @param sellerPk seller's PK (used for canceling the contract (spending the box).
    * @param tokenId token id seller wants to sell
    * @param tokenPrice price per token in nanoERGs
    * @param dexFeePerToken DEX matcher's reward (per token, nanoERGs)
    * The value of this sell order box is expected to be = tokenAmount * dexFeePerToken
    *
    * Requirements for inputs.
    * Matched buy counter order boxes should have the following properties:
    * Registers:
    * R4[Coll[Byte]] - token id of the sell order;
    * R5[Long] - token price of the sell order;
    * R6[Long] - dex fee per token;
    * Matched buy counter orders should be ranged in the inputs
    * by the "won" spread (spread = buyOrderTokenPrice - sellOrderTokenPrice if it's won
    * or spread = 0 if it's lost).
    *
    * Requirements for outputs.
    * Box with ERGs for sold tokens(return box).
    * There should be only one return box with the following properties:
    * - R4[Coll[Byte]] == this order box id;
    * - should be guarded only by sellerPk only;
    * - box value should be equal to soldTokenAmount * tokenPrice + spread(if any)
    * The spread is calculated by going through counter buy orders and taking the price difference
    * if this order is older(strict) than counter buy order (comparing creation height).
    *
    * If match is partial the new(residual) sell order box have to be in outputs.
    * There is can be only one such box in the outputs.
    * It should have the following registers set:
    * - R4[Coll[Byte]] = this sell order token id
    * - R5[Long] = this sell order `tokenPrice`
    * - R6[Long] = this sell order `dexFeePerToken`
    * - R7[Coll[Byte]] = this sell order box id
    * The residual box value should be (SELF.value - expectedDexFee)
    */
  def sellerContractInstance(parameters: DexSellerContractParameters): ErgoContract =
    DexLimitOrderErgoScript.sellerContract(parameters)

  def parseBuyerContractParameters(
    ergoTree: ErgoTree
  ): Option[DexBuyerContractParameters] =
    for {
      pk <- ergoTree.constants.headOption.collect {
             case SigmaPropConstant(ProveDlogProp(v)) => v
           }
      tokenId <- ergoTree.constants.lift(1).collect {
                  case ByteArrayConstant(coll) => coll.toArray
                }
      tokenPrice <- ergoTree.constants.lift(8).collect {
                     case Values.ConstantNode(value, SLong) => value.asInstanceOf[Long]
                   }
      dexFeePerToken <- ergoTree.constants.lift(22).collect {
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
      tokenId <- ergoTree.constants.lift(1).collect {
                  case ByteArrayConstant(coll) => coll.toArray
                }
      tokenPrice <- ergoTree.constants.lift(9).collect {
                     case Values.ConstantNode(value, SLong) => value.asInstanceOf[Long]
                   }
      dexFeePerToken <- ergoTree.constants.lift(22).collect {
                         case Values.ConstantNode(value, SLong) =>
                           value.asInstanceOf[Long]
                       }
    } yield DexSellerContractParameters(pk, tokenId, tokenPrice, dexFeePerToken)

  // remove after sigmastate v3.2.2+ is released and use ergoTree.template
  def ergoTreeTemplate(ergoTree: ErgoTree): Array[Byte] = {
    import sigmastate.serialization.SigmaSerializer
    import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
    val bytes = DefaultSerializer.serializeErgoTree(ergoTree)
    val r     = SigmaSerializer.startReader(bytes)
    DefaultSerializer.deserializeHeaderWithTreeBytes(r)._4
  }

  lazy val buyerContractErgoTreeTemplate: Array[Byte] = {
    val tokenId = Array.fill(ErgoBox.TokenId.size)(0.toByte)
    val pk      = ProveDlog(CryptoConstants.dlogGroup.createRandomElement())
    val params  = DexBuyerContractParameters(pk, tokenId, 2L, 2L)
    val c       = DexLimitOrderErgoScript.buyerContract(params)
    ergoTreeTemplate(c.ergoTree)
  }

  lazy val sellerContractErgoTreeTemplate: Array[Byte] = {
    val tokenId = Array.fill(ErgoBox.TokenId.size)(0.toByte)
    val pk      = ProveDlog(CryptoConstants.dlogGroup.createRandomElement())
    val params  = DexSellerContractParameters(pk, tokenId, 2L, 2L)
    val c       = DexLimitOrderErgoScript.sellerContract(params)
    ergoTreeTemplate(c.ergoTree)
  }

}
