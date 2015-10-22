package efa.nb.node

import efa.core.{Id, Name, Default, NamedL, UIdL, uidl, idL}
import efa.core.typeclass._
import efa.core.syntax.lens
import org.scalacheck.{Arbitrary, Gen}
import scalaz.Equal

case class Child(id: Id, name: Name)

object Child {
  implicit val equalInst: Equal[Child] = equal
  implicit val arbInst: Arbitrary[Child] = arbitrary
  implicit val defaultInst: Default[Child] = Default.derive
  implicit val uidInst: UIdL[Child] = uidl(idL[Child] >> 'id)
  implicit val namedInst: NamedL[Child] = NamedL lens (idL[Child] >> 'name)
}

// vim: set ts=2 sw=2 et:
