package efa.nb.node

import efa.core.{Default, NamedL, IntIdL, UniqueIdL}
import org.scalacheck.{Arbitrary, Gen}
import scalaz._, Scalaz._, scalacheck.ScalaCheckBinding._

case class Child(id: Int, name: String)

object Child {
  val idL: Child @> Int = Lens.lensu((a,b) ⇒ a copy (id = b), _.id)

  val nameL: Child @> String = Lens.lensu((a,b) ⇒ a copy (name = b), _.name)

  implicit val equalInst: Equal[Child] = Equal.equalA

  implicit val arbInst: Arbitrary[Child] = Arbitrary(
    ^(Gen choose (0,1000), Gen.identifier)(Child.apply)
  )

  implicit val defaultInst: Default[Child] = Default default Child(0,"")

  implicit val uidInst: IntIdL[Child] = UniqueIdL lens idL

  implicit val namedInst: NamedL[Child] = NamedL lens nameL
}

// vim: set ts=2 sw=2 et:
