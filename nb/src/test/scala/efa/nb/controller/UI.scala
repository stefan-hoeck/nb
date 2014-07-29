package efa.nb.controller

import dire._, DataSink.{sync, stdOut}, dire.control.Var, SF.id
import dire.util.test.runN
import dire.swing.UndoEdit
import efa.core.ValSt
import scalaz._, Scalaz._, effect._
import StateTrans.completeIsolated

object UI extends dire.util.TestFunctions {
  def run(es: Event*): List[Int] = simulate(es.toList, true)(sf)

  sealed trait Event
  case object Undo extends Event
  case object Redo extends Event
  case class Mod(f: Int ⇒ Int) extends Event

  private def sf(o: Out[Any]): IO[SF[Event,Int]] = for {
    v  ← Var newVar none[ValSt[Int]]
    sf ← IO {
           var us: List[UndoEdit] = Nil
           var rs: List[UndoEdit] = Nil
           var last = 0
           var notify = true

           def undo(u: UndoEdit) = 
             IO { rs = u :: rs; us = us.tail; notify = true } >> u.un

           def redo(u: UndoEdit) = 
             IO { us = u :: us; rs = rs.tail; notify = true } >> u.re

           //Only distinct events are passed on, therefor we
           //might need to push the `fired` button to notify
           //all interested parties, that the event in question was
           //indeed processed
           def onE(e: Event): IO[Unit] = e match {
             case Undo   ⇒ us.headOption.cata(undo, o(0))
             case Redo   ⇒ rs.headOption.cata(redo, o(0))
             case Mod(f) ⇒ (v put modify(f).success.some) >> IO {
                             val next = f(last)
                             val changed = next ≠ last
                             last = next

                             changed ? IO.ioUnit | o(0)
                           } μ
                           
           }

           def undoOut: Out[UndoEdit] = u ⇒ IO(us ::= u) >> o(0)

           def uiSF = v.in collectO identity

           def onLast(a: Any) = for {
             f ← IO(notify)
             _ ← if (f) IO(notify = false) >> o(a) else IO.ioUnit
           } yield ()

           (id[Event] syncTo onE) >>
           completeIsolated(uiSF.sf[Int], undoOut)(IO(0)).syncTo(onLast)
         }
  } yield sf

}

// vim: set ts=2 sw=2 et:
