# Ergo Contracts

## Prerequisites:
- Install Z3 SMT solver from https://github.com/Z3Prover/z3

## How to add a new contract

### Create a method for the contract
Subclass `SigmaContract` in the `verified-contracts` project and put a contract code in a method. The first parameter has to be `ctx: Context`, and subsequent parameters may be contract parameters. The return value has to be `SigmaProp`. Make the first line of the contract code `import ctx._` to improve readability.

### Write contract code in the method.
See [DEX buy order](http://github.com/ScorexFoundation/sigmastate-interpreter/blob/346717a7d6af45c685b617ccea7babcf39d49939/contract-verification/src/main/scala/sigmastate/verification/contract/AssetsAtomicExchange.scala#L12-L32) for an example.

### Contract compilation
Create a subclass (object) of the class with contract code to make "instance" method to compile the contract's code.
It'll invoke the compiler (macros) and returns a compiled contract with embedded contract parameters. Create a method with parameters from the contract (without the `Context` parameter) and invoke `ErgoContractCompiler.compile`. See [DEX buy order](http://github.com/ScorexFoundation/sigmastate-interpreter/blob/346717a7d6af45c685b617ccea7babcf39d49939/contract-verification/src/main/scala/sigmastate/verification/contract/AssetsAtomicExchange.scala#L116-L122) for an example.
Mark this method with `@ignore` annotation to hide it from Stainless. 

### How to use compiled contract
Call the "instance" method in another module/project and it'll return 'ErgoContract'(compiled contract).
Call `ErgoContract.scalaFunc` to run the contract with given `Context`. See [DEX buy order](http://github.com/ScorexFoundation/sigmastate-interpreter/blob/346717a7d6af45c685b617ccea7babcf39d49939/sigma-dsl-compiler-macros-playground/src/test/scala/sigmastate/verification/test/AssetsAtomicExchangeCompilationTest.scala#L177-L220) for an example.

### Verifying contract properties
Verification is done using [Stainless](https://epfl-lara.github.io/stainless/verification.html). Create a subclass(object) of the class where you put contracts (as methods). Use a method for each property you want to verify. Put pre-conditions in `require()` call, call the contract and verify post-conditions. See [DEX buy order](http://github.com/ergoplatform/ergo-contracts/blob/965d4dd8f4a13d4fd584c0088de271898c60deac/verified-contracts/src/main/scala/org/ergoplatform/contracts/AssetsAtomicExchange.scala#L90-L113) verified properties for an example.

