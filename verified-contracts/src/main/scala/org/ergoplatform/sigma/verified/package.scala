package org.ergoplatform.sigma

import stainless.annotation.ignore
import scala.language.implicitConversions

package object verified {

  @ignore
  implicit def implicitTo[A, B](a: A)(implicit iso: Iso[A, B]): B = iso.to(a)

  @ignore
  implicit def implicitFrom[A, B](b: B)(implicit iso: Iso[A, B]): A =
    InverseIso(iso).to(b)
}
