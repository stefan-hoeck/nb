package efa.nb.controller

import efa.react._
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect._

object StateTransTest extends Properties("StateTrans") {
  import StateTrans._

  property("basicIn_initial") = forAll{a: String ⇒ 
    val res = for {
      ui  ← UI basicIn a
      now ← ui.get
    } yield (now ≟ a) :| "initial value displayed by UI"

    eval(res)
  }

  property("basicIn_uiSet") = forAll{p: (String,String) ⇒ 
    val (a, b) = p

    val res = for {
      ui  ← UI basicIn a
      _   ← ui set b
      now ← ui.get
    } yield (now ≟ b) :| "new value displayed by UI"

    eval(res)
  }

  property("basicIn_uiMod") = forAll{p: (String,String) ⇒ 
    val (a, b) = p

    val res = for {
      ui  ← UI basicIn a
      _   ← ui mod (b + _)
      now ← ui.get
    } yield (now ≟ (b + a)) :| "start value modified and displayed"

    eval(res)
  }

  property("basicIn_uiModMod") = forAll{p: (String,String,String) ⇒ 
    val (a, b, c) = p

    val res = for {
      ui  ← UI basicIn a
      _   ← ui mod (b + _)
      _   ← ui mod (c + _)
      now ← ui.get
    } yield (now ≟ (c + b + a)) :| "start value modified and displayed"

    eval(res)
  }

  property("basicIn_uiModAfterDisplayChanged") = forAll{
    p: (String,String, String) ⇒ 
    val (a, b, c) = p

    val res = for {
      ui  ← UI basicIn a
      _   ← ui display b
      _   ← ui mod (c + _)
      now ← ui.get
    } yield (now ≟ (c + a)) :| "a updated, not b"

    eval(res)
  }

  property("basicIn_fail") = forAll{p: (String,String) ⇒ 
    val (a, b) = p

    val res = for {
      ui  ← UI basicIn a
      _   ← ui fail b
      now ← ui.get
    } yield (now ≟ a) :| "nothing changed"

    eval(res)
  }

  property("undoIn_initial") = forAll{a: String ⇒ 
    val res = for {
      ui  ← UI undoIn a
      now ← ui.get
    } yield (now ≟ a) :| "initial value displayed by UI"

    eval(res)
  }

  property("undoIn_uiSet") = forAll{p: (String,String) ⇒ 
    val (a, b) = p

    val res = for {
      ui  ← UI undoIn a
      _   ← ui set b
      now ← ui.get
    } yield (now ≟ b) :| "new value displayed by UI"

    eval(res)
  }

  property("undoIn_uiMod") = forAll{p: (String,String) ⇒ 
    val (a, b) = p

    val res = for {
      ui  ← UI undoIn a
      _   ← ui mod (b + _)
      now ← ui.get
    } yield (now ≟ (b + a)) :| "start value modified and displayed"

    eval(res)
  }

  property("undoIn_uiModMod") = forAll{p: (String,String,String) ⇒ 
    val (a, b, c) = p

    val res = for {
      ui  ← UI undoIn a
      _   ← ui mod (b + _)
      _   ← ui mod (c + _)
      now ← ui.get
    } yield (now ≟ (c + b + a)) :| "start value modified and displayed"

    eval(res)
  }

  property("undoIn_uiModAfterDisplayChanged") = forAll{
    p: (String,String, String) ⇒ 
    val (a, b, c) = p

    val res = for {
      ui  ← UI undoIn a
      _   ← ui display b
      _   ← ui mod (c + _)
      now ← ui.get
    } yield (now ≟ (c + a)) :| "a updated, not b"

    eval(res)
  }

  property("undoIn_fail") = forAll{p: (String,String) ⇒ 
    val (a, b) = p

    val res = for {
      ui  ← UI undoIn a
      _   ← ui fail b
      now ← ui.get
    } yield (now ≟ a) :| "nothing changed"

    eval(res)
  }

  property("undoIn_uiModUndo") = forAll{p: (String,String) ⇒ 
    val (a, b) = p

    val res = for {
      ui  ← UI undoIn a
      _   ← ui mod (b + _)
      _   ← ui.undo
      now ← ui.get
    } yield (now ≟ a) :| "edit undone"

    eval(res)
  }

  property("undoIn_uiModUndoSignalValue") = forAll{p: (String,String) ⇒ 
    val (a, b) = p

    val res = for {
      ui  ← UI(a)
      undoInS = undoIn(ui.stTrans, ui.undoOut)(IO(a))
      p   ← undoInS.go
      _   ← ui mod (b + _)
      _   ← ui.undo
      now ← p._2.now
    } yield (now ≟ a) :| "edit undone"

    eval(res)
  }

  property("undoIn_uiModUndoRedo") = forAll{p: (String,String) ⇒ 
    val (a, b) = p

    val res = for {
      ui  ← UI undoIn a
      _   ← ui mod (b + _)
      _   ← ui.undo
      _   ← ui.redo
      now ← ui.get
    } yield (now ≟ (b + a)) :| "edit redone"

    eval(res)
  }

  val g = for {
    a ← Gen.identifier
    b ← Gen.identifier
  } yield (a,b)

  property("undoIn_uiModUndoMod") = forAll(g){p ⇒ 
    val (a, b) = p

    val res = for {
      ui  ← UI undoIn a
      _   ← ui mod (b + _)
      _   ← ui.undo
      _   ← ui mod (b + _)
      now ← ui.get
    } yield (now ≟ (b + a)) :| s"exp: ${b + a} but was: $now"

    eval(res)
  }

  def eval (io: IO[Prop]): Prop = io.unsafePerformIO
}

// vim: set ts=2 sw=2 et:
