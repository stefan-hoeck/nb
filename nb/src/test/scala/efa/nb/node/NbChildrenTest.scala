package efa.nb.node

import efa.core.{UniqueId, Efa, ValSt, ParentL}, Efa._
import efa.react._
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO

object NbChildrenTest extends Properties("NbChildren") {
  import NbChildren._

  private val toChild = (p: (String,Int)) ⇒ Child (p._2, p._1)

  //Generators
  val strG = Gen.identifier
  val childrenGen: Gen[List[Child]] =
    Gen listOf strG map (_.zipWithIndex map toChild)
  implicit val ChildrenArbitrary = Arbitrary (childrenGen)

  lazy val nameOut = NbNode.name[Child](_.name)
  lazy val listFac = leavesF(nameOut)(identity[List[Child]])
  lazy val seqOut = children(listFac)

  property ("seqFactory") = forAll { cs: List[Child] ⇒ 
    val res = for {
      n ← NbNode.apply
      _ ← seqOut set n runIO Signal.newVal(cs)
      names = displayNames (n)
    } yield names ≟ cs.map (_.name)

    eval (res)
  }

  //Test that after resetting the nodes, all nodes are created afresh
  property ("seqFactory_reset") = Prop.forAll { cs: List[Child] ⇒ 
    val rs = cs map (c ⇒ Child (c.id, c.name.reverse))

    val res = for {
      n ← NbNode.apply
      v ← Signal newVar cs
      _ ← seqOut set n runIO v
      ca = n.getChildren.getNodes
      aSet = (displayNames (n) ≟ cs.map (_.name)) :| "first set"
      _ ← v put rs
      cb = n.getChildren.getNodes
      bSet = (displayNames (n) ≟ rs.map (_.name)) :| "second set"
      ne = (ca zip cb).toList ∀ {case (a,b) ⇒ a ne b} :| "equality"
    } yield aSet && bSet && ne

    evalProp (res)
  }

  lazy val uidFac = uidF(nameOut)(identity[List[Child]])
  lazy val uidOut = children(uidFac)

  property ("uidFactory") = Prop.forAll { cs: List[Child] ⇒ 
    val res = for {
      n ← NbNode.apply
      _ ← uidOut set n runIO Signal.newVal(cs)
      names = displayNames (n)
    } yield names ≟ cs.map (_.name)

    eval (res)
  }

  //Test that after resetting the nodes, the same nodes are used if
  //The id numbers stay the same
  property ("uidFactory_reset") = Prop.forAll { cs: List[Child] ⇒ 
    val rs = cs map (c ⇒ Child (c.id, c.name.reverse))

    val res = for {
      n ← NbNode.apply
      v ← Signal newVar cs
      _ ← uidOut set n runIO v
      ca = n.getChildren.getNodes
      aSet = (displayNames (n) ≟ cs.map (_.name)) :| "first set"
      _ ← v put rs
      cb = n.getChildren.getNodes
      bSet = (displayNames (n) ≟ rs.map (_.name)) :| "second set"
      eq = (ca zip cb).toList ∀ {case (a,b) ⇒ a eq b} :| "equality"
    } yield aSet && bSet && eq

    evalProp (res)
  }

  //HList-based
  type NPOut[A] = NodeOut[A,ValSt[Parent]]

  val childOut: NPOut[FullChild] =
    (NbNode.destroyP: NPOut[FullChild]) ⊹ 
    NbNode.editDialogP ⊹ 
    NbNode.named

  val parentOut: NPOut[ParentPath] = 
    children(parentF(childOut)) ⊹
    NbNode.named
  
  private def displayNames (n: NbNode): List[String] =
    n.getChildren.getNodes(true).toList map (_.getDisplayName)

  private def eval (io: IO[Boolean]) = io.unsafePerformIO

  private def evalProp (io: IO[Prop]) = io.unsafePerformIO

}

// vim: set ts=2 sw=2 et:
