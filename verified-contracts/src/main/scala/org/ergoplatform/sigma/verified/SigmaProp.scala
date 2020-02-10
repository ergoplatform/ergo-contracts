package org.ergoplatform.sigma.verified

import scalan.OverloadId
import sigmastate.interpreter.CryptoConstants.EcPointType
import stainless.annotation.{extern, library}

// TODO: extract

/** Proposition which can be proven and verified by sigma protocol. */
@scalan.Liftable
@library
sealed trait SigmaProp {
  def isValid: Boolean

  /** Serialized bytes of this sigma proposition taken as ErgoTree and then serialized. */
  def propBytes: Coll[Byte]

  /** Logical AND between this SigmaProp and other SigmaProp.
    * This constructs a new CAND node of sigma tree with two children. */
  @OverloadId("and_sigma") def &&(other: SigmaProp): SigmaProp

  /** Logical AND between this `SigmaProp` and `Boolean` value on the right.
    * The boolean value will be wrapped into `SigmaProp` using `sigmaProp` function.
    * This constructs a new CAND node of sigma tree with two children. */
  @OverloadId("and_bool") def &&(other: Boolean): SigmaProp

  /** Logical OR between this SigmaProp and other SigmaProp.
    * This constructs a new COR node of sigma tree with two children. */
  @OverloadId("or_sigma") def ||(other: SigmaProp): SigmaProp

  /** Logical OR between this `SigmaProp` and `Boolean` value on the right.
    * The boolean value will be wrapped into `SigmaProp` using `sigmaProp` function.
    * This constructs a new COR node of sigma tree with two children. */
  @OverloadId("or_bool") def ||(other: Boolean): SigmaProp
}

@library
trait SigmaBoolean {}

@library
case class TrivialProp(condition: Boolean) extends SigmaBoolean {}

@library
case class SigmaPropProof(sigmaTree: SigmaBoolean) extends SigmaProp {

  override def isValid: Boolean = sigmaTree match {
    case p: TrivialProp => p.condition
    case _              => false
  }

  override def propBytes: Coll[Byte] = ???

  override def &&(other: SigmaProp): SigmaProp = (isValid, other.isValid) match {
    case (true, true) => SigmaPropProof(TrivialProp(true))
    case _            => SigmaPropProof(TrivialProp(false))
  }

  override def &&(other: Boolean): SigmaProp = (isValid, other) match {
    case (true, true) => SigmaPropProof(TrivialProp(true))
    case _            => SigmaPropProof(TrivialProp(false))
  }

  override def ||(other: SigmaProp): SigmaProp = (isValid, other.isValid) match {
    case (false, o) => SigmaPropProof(TrivialProp(o))
    case (true, _)  => SigmaPropProof(TrivialProp(true))
  }

  override def ||(other: Boolean): SigmaProp = (isValid, other) match {
    case (false, o) => SigmaPropProof(TrivialProp(o))
    case (true, _)  => SigmaPropProof(TrivialProp(true))
  }
}

@library
case class ProveDlogProof(@extern value: EcPointType) extends SigmaBoolean {}
