package efa.nb

import efa.core.{UniqueId, Named}
import efa.nb.dialog.{DialogEditable ⇒ DE}
import shapeless.{HNil, ::}
import scalaz.Show

package object node {
  type ParentPath = Parent :: HNil
  type FullChild = Child :: ParentPath

  implicit def namedInst: Named[FullChild] = implicitly
  implicit val FullChildDE: DE[FullChild,Child] = DE.io1 { f ⇒ ??? }
}

// vim: set ts=2 sw=2 et:
