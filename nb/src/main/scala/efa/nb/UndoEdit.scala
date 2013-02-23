package efa.nb

import efa.react._
import org.openide.awt.UndoRedo
import javax.swing.event.UndoableEditEvent
import javax.swing.undo.AbstractUndoableEdit
import scalaz._, Scalaz._, effect.IO

case class UndoEdit (un: IO[Unit], re: IO[Unit])
   extends AbstractUndoableEdit {
  override def undo() {super.undo(); un.unsafePerformIO}
  override def redo() {super.redo(); re.unsafePerformIO}
}

object UndoEdit extends IOCFunctions {
  def pairs[A]: SET[A,(A,A)] =
    sTrans.id[A].fpair reduce ((a,b) ⇒ (a._1,b._1)) events

  def toUndoEdit[A] (out: Out[A])(p: (A,A)): UndoEdit =
    UndoEdit(out(p._2), out(p._1))

  def undoEET[A] (out: Out[UndoEdit]): EET[(A,A),A] =
    eTrans.id[(A,A)] blockOn eTrans(aas ⇒ 
      for {
        src ← Events.srcC[A]
        _   ← aas map toUndoEdit(src.fire) flatMap (_ --> out)
      } yield src
    )

  def undoSST[A] (out: Out[UndoEdit]): SST[A,A] =
    eTrans loopHold (pairs[A] >=> undoEET(out)) append
    sTrans.id[A].events

  def managedOut (manager: UndoRedo.Manager): Out[UndoEdit] = {
    def event (ue: UndoEdit) = new UndoableEditEvent (this, ue)
    ue ⇒ IO(manager.undoableEditHappened (event (ue)))
  }

  def undoManager (uds: EIn[UndoEdit]):IOC[UndoRedo] = for {
    udm ← point (new UndoRedo.Manager)
    _   ← uds to managedOut(udm) run ()
  } yield udm
}

// vim: set ts=2 sw=2 et:
