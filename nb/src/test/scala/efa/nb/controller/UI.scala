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
           var last = 0

           def undo(u: UndoEdit) = u.un >> IO { rs = u :: rs; us = us.tail }
           def redo(u: UndoEdit) = u.re >> IO { us = u :: us; rs = rs.tail }

           //Only distinct events are passed on, therefor we
           //might need to push the `fired` button to notify
           //all interested parties, that the event in question was
           //indeed processed
           def onE(e: Event): IO[Unit] = e match {
             case Undo   ⇒ us.headOption.cata(undo, o(()))
             case Redo   ⇒ rs.headOption.cata(redo, o(()))
             case Mod(f) ⇒ (v put modify(f).success.some) >> IO {
                             val next = f(last)
                             val changed = next ≠ last
                             last = next

                             changed ? IO.ioUnit | o(())
                           } μ
                           
           }

           def undoOut: Out[UndoEdit] = u ⇒ IO(us ::= u)

           def uiSF = v.in collectO identity

           (id[Event] syncTo onE) >>
           completeIsolated(uiSF.sf, undoOut, Sequential)(IO(0))
         }
  } yield sf
}

// vim: set ts=2 sw=2 et:
