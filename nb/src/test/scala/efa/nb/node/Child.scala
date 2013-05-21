package efa.nb.node

import efa.core._
import org.scalacheck.{Arbitrary, Gen}
import scalaz._, Scalaz._, scalacheck.ScalaCheckBinding._

case class Child(id: Int, name: String)

object Child {
  implicit lazy val ChildEqual = Equal.equalA[Child]

  implicit lazy val ChildArbitrary: Arbitrary[Child] = Arbitrary (
    ^(Gen choose (0, 1000), Gen.identifier)(Child.apply))

  implicit lazy val ChildData = 
    new NamedL[Child]
    with Default[Child]
    with UniqueIdL[Child,Int] {
      val nameL = Child.name
      val idL = Child.id
      val default = Child(0, "child")
    }

  val name: Child @> String = Lens.lensu((a,b) ⇒ a.copy(name = b), _.name)
  val id: Child @> Int = Lens.lensu((a,b) ⇒ a.copy(id = b), _.id)
}

// vim: set ts=2 sw=2 et:
