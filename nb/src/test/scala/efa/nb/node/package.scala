package efa.nb

import efa.nb.dialog.{DialogEditable ⇒ DE}
import shapeless.{HNil, ::}

package object node {
  type ParentPath = Parent :: HNil
  type FullChild = Child :: ParentPath

  implicit val FullChildDE: DE[FullChild,Child] = new DE[FullChild,Child] {
    def component(f: FullChild, isCreate: Boolean) = ???
    def signalIn(c: Comp) = ???
  }
}

// vim: set ts=2 sw=2 et:
