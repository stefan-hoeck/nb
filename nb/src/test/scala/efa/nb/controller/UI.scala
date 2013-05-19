package efa.nb.controller

import dire._, DataSink.sync, dire.control.Var, SF.id
import dire.util.test.runN
import dire.swing.UndoEdit
import efa.core.ValSt
import scalaz._, Scalaz._, effect._
import scalaz.std.indexedSeq._
import scalaz.concurrent.Strategy.Sequential
import StateTrans.completeIsolated

object UI {
  def run(es: Event*): List[Int] = runN(SF io sf(es: _*), es.size + 1)

  sealed trait Event
  case class Mod(f: Int ⇒ Int) extends Event
  case object Undo extends Event
  case object Redo extends Event
  
  /** Simulates a user interface where the events given
    * by param `es` are fired sequentially.
    *
    * The returned list contains all values displayed in
    * the user interface (with an initial value of 0).
    */
  private def sf(es: Event*): IO[SIn[Int]] = for {
    v  ← Var newVar none[Int]
    sf ← IO {
           val strategy = Some(Sequential)
           val esA = es.toArray
           var us: List[UndoEdit] = Nil
           var rs: List[UndoEdit] = Nil

           def undo(u: UndoEdit) = u.un >> IO { rs = u :: rs; us = us.tail }
           def redo(u: UndoEdit) = u.re >> IO { us = u :: us; rs = rs.tail }

           def onE(e: Event): IO[Unit] = e match {
             case Undo   ⇒ us.headOption map undo orZero
             case Redo   ⇒ rs.headOption map redo orZero
             case _      ⇒ IO.ioUnit
           }

           def undoOut: Out[UndoEdit] = u ⇒ IO(us ::= u)
           def values = v.in collectO identity //values displayed in the UI

           //input events are fired sequentially whenever a value has been
           //displayed in the UI
           def input = values.count.events
                             .filter { esA.size >= }
                             .map { i ⇒ esA(i - 1) }

           def uiSF = (id[Int] syncTo { v put _.some }) >>
                      (input syncTo onE collect { case Mod(f) ⇒ modify(f).success })

           completeIsolated(uiSF, undoOut, strategy)(IO(0)) >> values
         }
  } yield sf
}

// vim: set ts=2 sw=2 et:
