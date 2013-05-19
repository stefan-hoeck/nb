package efa.nb.controller

import dire._
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO

object StateTransTest extends Properties("StateTrans") {
  import UI._

  property("no events") = UI.run() ≟ List(0)

  property("add one") = forAll { i: Int ⇒ 
    UI.run(Mod(i + _)) ≟ List(0, i)
  }

  property("add two") = forAll { p: (Int,Int) ⇒ 
    val (a,b) = p

    UI.run(Mod(a+), Mod(b*)) ≟ List(0, a, a * b)
  }

  property("undo one") = forAll { i: Int ⇒ 
    UI.run(Mod(i+), Undo) ≟ List(0, i, 0)
  }

  property("add two, undo two") = forAll { p: (Int,Int) ⇒ 
    val (a,b) = p

    UI.run(Mod(a+), Mod(b*), Undo, Undo) ≟ List(0, a, a * b, a, 0)
  }

  property("undo redo") = forAll { i: Int ⇒ 
    UI.run(Mod(i+), Undo, Redo) ≟ List(0, i, 0, i)
  }
}

// vim: set ts=2 sw=2 et:
