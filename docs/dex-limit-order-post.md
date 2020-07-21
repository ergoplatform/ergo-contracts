# Decentralized Exchange on Ergo

Ergo have expressive smart contracts and transaction model which allows the implementation of trustless DEX protocol, in which signed buy and sell orders can be put into the blockchain independently by buyers and sellers. An off-chain matching service can observe the Ergo blockchain, find matching orders, and submit the swap transaction without knowing any secrets. The matching can be incentivized by DEX reward paid as part of a swap transaction. Anyone who first discover the match of the two orders can create the swap transaction and get a reward in ERGs. Partial matching is supported, meaning that target (buy/sell) order can be executed partially, in which case a new "residual" order(box) has to be created in the same swap transaction. Any order can be canceled anytime by the "owner."
Both sell and buy order contracts can be found at [src](https://github.com/ergoplatform/ergo-contracts/blob/4081e4909e4e1882fd2bfc52cd37da1609e32733/contracts/src/main/scala/org/ergoplatform/contracts/DexLimitOrder.scala#L45-L315)

## Partial matching
Both contracts have token price and DEX fee parameters encoded on a compilation. This allows us to check the "residual" order assets, ERGs for a buy order, and tokens for a sell order.


In the buy order contract, we search for a residual box, checking that it has correct parameters and assets.
```
// in case of partial matching new buy order box should be created with funds that are not matched in this tx
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
```
Then, we check that the following properties hold:
- Value (ERGs) of the "residual" order box is the value of the current box(order) minus ERGs value of the tokens we're receiving in this swap transaction and minus the DEX fee for this swap transaction.
- Only one "residual" order box is created in this swap transaction.

```
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
```

In the sell order contract, we search for a residual box, checking that it has correct parameters and assets.

```
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
```

Then, we check that the following properties hold:
- The difference between the token amount in the current box(order) and the "residual" order box determines the amount of ERGs seller receives for the tokens "sold" in this swap transaction (`soldTokenAmount * tokenPrice`).
- Value (ERGs) of the "residual" order box is the value of the current box(order) minus the DEX fee for this swap transaction.
- Only one "residual" order box is created in this swap transaction.

```
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
```

## Total matching
Both sell and buy orders can be executed in the swap transaction entirely. In this case, there is no requirement for the "residual" order box. 
For this path, we check that the following properties hold.
For sell order:
- ERGs amount seller receives in this swap transaction have to be equal to amount of tokens in the current order times token price.
`val totalMatching = (returnBox.value == selfTokenAmount * tokenPrice + fullSpread(selfTokenAmount))`

For buy order:
- Token value (token amount * token price, in ERGs) buyer receives in this swap transaction have to be equal to the value of the current box(order) minus DEX fee.
`val totalMatching = (SELF.value - expectedDexFee) == (returnTokenAmount * tokenPrice) && returnBox.value >= fullSpread`

## Spread 
TBD

