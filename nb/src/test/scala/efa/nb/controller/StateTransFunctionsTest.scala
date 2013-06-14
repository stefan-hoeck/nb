package efa.nb.controller

import dire._
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO

object StateTransTest extends Properties("StateTrans") {
  import UI._

  property("no events") = UI.run() ≟ dist(0)

  property("add one") = forAll { i: Int ⇒ 
    UI.run(Mod(i + _)) ≟ dist(0, i)
  }

  property("add two") = forAll { p: (Int,Int) ⇒ 
    val (a,b) = p

    UI.run(Mod(a+), Mod(b*)) ≟ dist(0, a, a * b)
  }

  property("undo one") = forAll { i: Int ⇒ 
    val res = UI.run(Mod(i+), Undo)
    val exp = dist(0, i, 0)

    (res ≟ exp) :| s"exp: $exp but was $res"
  }

  property("add two, undo two") = forAll { p: (Int,Int) ⇒ 
    val (a,b) = p

    UI.run(Mod(a+), Mod(b*), Undo, Undo) ≟ dist(0, a, a * b, a, 0)
  }

  property("undo redo") = forAll { i: Int ⇒ 
    UI.run(Mod(i+), Undo, Redo) ≟ dist(0, i, 0, i)
  }

  property("complex undo redo") = {
    val (a, b, c) = (10, 20, -30)

    UI.run(Mod(a+), Mod(b+), Undo, Mod(c+), Undo, Undo, Redo, Mod(a+)) ≟ 
      dist(0, a, a + b, a, a + c, a, 0, a, a + a)
  }

  import UINode.Rename

  property("ui with node") = {
    UINode.run(Rename("blub"), Rename("blib"), Rename("blab")) ≟
      List("boo", "blub", "blib", "blab")
  }

  type ISt[+A] = State[Int,A]

  //Creates a list of distinct values
  def dist(is: Int*): List[Int] = {
    val iL = is.toList
    val ini = iL.headOption.cata(_ - 1, 0)

    def st(i: Int): ISt[Option[Int]] =
      State { old ⇒ (i, (old ≟ i) ? none[Int] | i.some) }

    iL traverse st eval ini flatten
  }
}

// vim: set ts=2 sw=2 et:
