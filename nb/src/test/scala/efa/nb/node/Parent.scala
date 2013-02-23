package efa.nb.node

import efa.core._, efa.core.{Parent ⇒ CParent}
import org.scalacheck.{Arbitrary, Gen}
import scalaz._, Scalaz._, scalacheck.ScalaCheckBinding._
import shapeless.{HNil, ::}

case class Parent(id: Int, name: String, children: List[Child])

object Parent {
  implicit lazy val ParentEqual = Equal.equalA[Parent]

  implicit lazy val ParentArbitrary: Arbitrary[Parent] = Arbitrary (
    ^^(
      Gen choose (0, 1000),
      Gen.identifier,
      Gen listOf Arbitrary.arbitrary[Child])(Parent.apply)
  )

  implicit lazy val ParentData = 
    new NamedL[Parent]
    with Default[Parent]
    with UniqueIdL[Parent,Int] {
      val nameL = Parent.name
      val idL = Parent.id
      val default = Parent(0, "Parent", Nil)
      def children(p: Parent) = p.children map (_ :: p :: HNil)
    }

  implicit lazy val PParentL = ParentL mplusRoot children

  val name: Parent @> String = Lens.lensu((a,b) ⇒ a.copy(name = b), _.name)

  val id: Parent @> Int = Lens.lensu((a,b) ⇒ a.copy(id = b), _.id)

  val children: Parent @> List[Child] =
    Lens.lensu((a,b) ⇒ a.copy(children = b), _.children)
  
}

// vim: set ts=2 sw=2 et:
