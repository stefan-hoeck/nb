package efa.nb.node

import efa.core.{Default, NamedL, IntIdL, UniqueIdL, ParentL}
import org.scalacheck.{Arbitrary, Gen}
import scalaz._, Scalaz._, scalacheck.ScalaCheckBinding._

case class Parent(id: Int, name: String, children: List[Child])

object Parent {
  val idL: Parent @> Int = Lens.lensu((a,b) ⇒ a copy (id = b), _.id)

  val nameL: Parent @> String = Lens.lensu((a,b) ⇒ a copy (name = b), _.name)

  val childrenL: Parent @> List[Child] =
    Lens.lensu((a,b) ⇒ a copy (children = b), _.children)

  implicit val equalInst: Equal[Parent] = Equal.equalA

  implicit val arbInst: Arbitrary[Parent] = Arbitrary(
    ^^(Gen choose (0,1000),
       Gen.identifier,
       Arbitrary.arbitrary[List[Child]]
      )(Parent.apply)
  )

  implicit val defaultInst: Default[Parent] = Default default Parent(0,"",Nil)

  implicit val uidInst: IntIdL[Parent] = UniqueIdL lens idL

  implicit val namedInst: NamedL[Parent] = NamedL lens nameL

  implicit val parentLInst: ParentL[List,Parent,Child,ParentPath] =
    ParentL mplusRoot childrenL
}

// vim: set ts=2 sw=2 et:
