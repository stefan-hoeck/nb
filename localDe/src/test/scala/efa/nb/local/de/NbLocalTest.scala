package efa.nb.local.de

import org.scalacheck._, Prop._

object NbLocalTest extends Properties("NbLocal") {

  property("registered") = efa.nb.loc.isInstanceOf[NbLocal]

}

// vim: set ts=2 sw=2 et:
