package efa.nb

import efa.core.{UniqueId, Named}
import efa.nb.dialog.{DialogEditable ⇒ DE}
import shapeless.{HNil, ::}
import scalaz.Show

package object node {
  type ParentPath = Parent :: HNil
  type FullChild = Child :: ParentPath

  implicit val FullChildShow: Show[FullChild] = Show shows { _.head.name }
  implicit val FullChildUId: UniqueId[FullChild,Int] = UniqueId.get(_.head.id)
  implicit val FullChildDE: DE[FullChild,Child] = 
    DE.io1[FullChild,Child] { f ⇒ ??? }
}

// vim: set ts=2 sw=2 et:
