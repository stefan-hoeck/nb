package efa.nb.node

import efa.core.{Id, Name, Default, NamedL, UIdL, uidl, idL, ParentL}
import efa.core.typeclass._
import efa.core.syntax.lens
import org.scalacheck.{Arbitrary, Gen}
import scalaz.Equal
import scalaz.std.list._

case class Parent(id: Id, name: Name, children: List[Child])

object Parent {
  implicit val cdefault: Default[List[Child]] = Default default Nil
  implicit val equalInst: Equal[Parent] = equal
  implicit val arbInst: Arbitrary[Parent] = arbitrary
  implicit val defaultInst: Default[Parent] = Default.derive
  implicit val uidInst: UIdL[Parent] = uidl(idL[Parent] >> 'id)
  implicit val namedInst: NamedL[Parent] = NamedL lens (idL[Parent] >> 'name)
  implicit val parentLInst: ParentL[List,Parent,Child,ParentPath] =
    ParentL mplusRoot (idL[Parent] >> 'children)
}

// vim: set ts=2 sw=2 et:
