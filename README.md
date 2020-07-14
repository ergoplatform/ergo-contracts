# Ergo Contracts
Source code of the Ergo smart contracts with compilation, testing, and formal verification tooling.

# List of contracts:
Certified(ErgoScala):
- Assets Atomic Exchange [src](https://github.com/ergoplatform/ergo-contracts/blob/19e23fde8c94a71eb8ca839c829e4efb4e4e63ac/verified-contracts/src/main/scala/org/ergoplatform/contracts/AssetsAtomicExchange.scala#L12-L70) , [verified properties](https://github.com/ergoplatform/ergo-contracts/blob/19e23fde8c94a71eb8ca839c829e4efb4e4e63ac/verified-contracts/src/main/scala/org/ergoplatform/contracts/AssetsAtomicExchange.scala#L72-L141)
- Crowd Funding [src](https://github.com/ergoplatform/ergo-contracts/blob/19e23fde8c94a71eb8ca839c829e4efb4e4e63ac/verified-contracts/src/main/scala/org/ergoplatform/contracts/CrowdFundingContractVerification.scala#L10-L30), [verified properties](https://github.com/ergoplatform/ergo-contracts/blob/19e23fde8c94a71eb8ca839c829e4efb4e4e63ac/verified-contracts/src/main/scala/org/ergoplatform/contracts/CrowdFundingContractVerification.scala#L32-L105)
- ICO Funding [src](https://github.com/ergoplatform/ergo-contracts/blob/19e23fde8c94a71eb8ca839c829e4efb4e4e63ac/verified-contracts/src/main/scala/org/ergoplatform/contracts/ICOContractVerification.scala#L9-L186), [verified properties](https://github.com/ergoplatform/ergo-contracts/blob/19e23fde8c94a71eb8ca839c829e4efb4e4e63ac/verified-contracts/src/main/scala/org/ergoplatform/contracts/ICOContractVerification.scala#L208-L279)

ErgoScript:
- DEX with limit orders and partial matching [src](https://github.com/ergoplatform/ergo-contracts/blob/19e23fde8c94a71eb8ca839c829e4efb4e4e63ac/contracts/src/main/scala/org/ergoplatform/contracts/DexLimitOrder.scala#L45-L315) [docs](https://github.com/ergoplatform/ergo-contracts/blob/19e23fde8c94a71eb8ca839c829e4efb4e4e63ac/contracts/src/main/scala/org/ergoplatform/contracts/DexLimitOrder.scala#L319-L397)

## How to add a new certified contract
Certified contracts are written in ErgoScala (a subset of Scala, compiled with [ErgoScala compiler](https://github.com/ergoplatform/ergo-scala-compiler)) and have their properties verified using formal verification with [Stainless](https://stainless.epfl.ch/).

## Prerequisites for certified contracts:
- Install Z3 SMT solver from https://github.com/Z3Prover/z3

### Create a method for the contract
Subclass `SigmaContract` in the `verified-contracts` project and put a contract code in a method. The first parameter has to be `ctx: Context`, and subsequent parameters may be contract parameters. The return value has to be `SigmaProp`. Make the first line of the contract code `import ctx._` to improve readability.

### Write contract code in the method.
See [DEX buy order](http://github.com/ergoplatform/ergo-contracts/blob/71f1ef745b7ffce80272e7050a65ec4f68bfd661/verified-contracts/src/main/scala/org/ergoplatform/contracts/AssetsAtomicExchange.scala#L12-L45) for an example.

### Contract compilation (ErgoScala)
Create a subclass (object) of the class with contract code to make an "instance" method to compile the contract's code.
It'll invoke the compiler (macros) and returns a compiled contract with embedded contract parameters. Create a method with parameters from the contract (without the `Context` parameter) and invoke `ErgoContractCompiler.compile`. See [DEX buy order](http://github.com/ergoplatform/ergo-contracts/blob/71f1ef745b7ffce80272e7050a65ec4f68bfd661/verified-contracts/src/main/scala/org/ergoplatform/contracts/AssetsAtomicExchange.scala#L150-L158) for an example.
Mark this method with `@ignore` annotation to hide it from Stainless. 

### How to use compiled contract
Call the "instance" method in another module/project, and it'll return 'ErgoContract'(compiled contract).
Call `ErgoContract.scalaFunc` to run the contract with given `Context`. See [DEX buy order](http://github.com/ergoplatform/ergo-contracts/blob/71f1ef745b7ffce80272e7050a65ec4f68bfd661/verified-contracts-test/src/test/scala/org/ergoplatform/contracts/tests/AssetsAtomicExchangeCompilationTest.scala#L319-L324) for an example.

### Verifying contract properties
Verification is done using [Stainless](https://epfl-lara.github.io/stainless/verification.html). Create a subclass(object) of the class where you put contracts (as methods). Use a method for each property you want to verify. Put pre-conditions in `require()` call, call the contract, and verify post-conditions. See [DEX buy order](http://github.com/ergoplatform/ergo-contracts/blob/71f1ef745b7ffce80272e7050a65ec4f68bfd661/verified-contracts/src/main/scala/org/ergoplatform/contracts/AssetsAtomicExchange.scala#L90-L113) verified properties for an example.

