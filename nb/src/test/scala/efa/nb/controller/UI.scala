package efa.nb.controller

import dire._, DataSink.{sync, stdOut}, dire.control.Var, SF.id
import dire.util.test.runN
import dire.swing.UndoEdit
import efa.core.ValSt
import scalaz._, Scalaz._, effect._
import scalaz.std.indexedSeq._
import scalaz.concurrent.Strategy.Sequential
import StateTrans.completeIsolated

object UI extends dire.util.TestFunctions {
  def run(es: Event*): List[Int] = simulate(es.toList, true)(sf)

  sealed trait Event
  case object Undo extends Event
  case object Redo extends Event
  case class Mod(f: Int ⇒ Int) extends Event

  private def sf(o: Out[Unit]): IO[SF[Event,Int]] = for {
    v  ← Var newVar none[ValSt[Int]]
    sf ← IO {
           var us: List[UndoEdit] = Nil
           var rs: List[UndoEdit] = Nil

           def undo(u: UndoEdit) = u.un >> IO { rs = u :: rs; us = us.tail }
           def redo(u: UndoEdit) = u.re >> IO { us = u :: us; rs = rs.tail }

           def onE(e: Event): IO[Unit] = e match {
             case Undo   ⇒ us.headOption map undo orZero
             case Redo   ⇒ rs.headOption map redo orZero
             case Mod(f) ⇒ IO.putStrLn(Mod(f).toString) >>
                           (v put modify(f).success.some)
           }

           def undoOut: Out[UndoEdit] = u ⇒ IO(us ::= u)

           def uiSF = (id[Int] to stdOut) >> (v.in collectO identity)

           (id[Event] syncTo onE) >>
           completeIsolated(uiSF, undoOut)(IO(0))
         }
  } yield sf
}

// vim: set ts=2 sw=2 et:
