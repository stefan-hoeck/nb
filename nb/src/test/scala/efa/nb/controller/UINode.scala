package efa.nb.controller

import dire._, DataSink.{sync, stdOut}, dire.control.Var, SF.id
import dire.util.test.runN
import dire.swing.UndoEdit
import efa.core.ValSt
import efa.nb.node.{NbNode, NodeOut}
import scalaz._, Scalaz._, effect.IO
import StateTrans.completeIsolated

object UINode extends dire.util.TestFunctions {
  def run(es: Event*): List[String] =
    simulate(es.toList, true)(sf)

  val out: NodeOut[String,String] = 
    (NbNode.rename: NodeOut[String,String]) ⊹ NbNode.name(identity)

  sealed trait Event
  case class Rename(s: String) extends Event

  private def sf(o: Out[Unit]): IO[SF[Event,String]] = for {
    n  ← NbNode()
    sf ← IO {
           def onE(e: Event): IO[Unit] = e match {
             case Rename(s)   ⇒ 
               IO.putStrLn(n.getDisplayName) >>
               IO(n.setName(s))
           }

           def undoOut: Out[UndoEdit] = u ⇒ IO.ioUnit

           def uiSF: SF[String,ValSt[String]] =
             out sfST (n, Sequential) map (put(_).success)

           (id[Event] syncTo onE) >>
           completeIsolated(uiSF, undoOut, Sequential)(IO("boo"))
         }
  } yield sf
}

// vim: set ts=2 sw=2 et:
