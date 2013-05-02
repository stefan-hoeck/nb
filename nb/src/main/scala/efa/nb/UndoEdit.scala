package efa.nb

import dire._, SF.{id, loop, connectOuts}
import org.openide.awt.UndoRedo
import javax.swing.event.UndoableEditEvent
import javax.swing.undo.AbstractUndoableEdit
import scalaz._, Scalaz._, effect.IO
import scalaz.concurrent.Strategy.{SwingInvokeLater ⇒ SIL}

final case class UndoEdit (un: IO[Unit], re: IO[Unit])
   extends AbstractUndoableEdit {
  override def undo() {super.undo(); un.unsafePerformIO}
  override def redo() {super.redo(); re.unsafePerformIO}
}

object UndoEdit {
  //Convention: Rights come from new input, Lefts from Undo, Redo
  //First option is old value, second option is new value
  type UEDis[+A] = (Option[A],Option[A]) \/ (Option[A],Option[A]) 

  private[nb] def ini[A]: UEDis[A] = (None, None).left

  //Accumulates Undo/Redo pairs. As soon as an event stream fired
  //two events, we have a pair of an old and a new value. This
  //is represented by a pair of Options. The latest event might
  //have come from user input in which case it is wrapped in a Right
  //and the resulting pair of options is also wrapped in a right. If
  //the lates event comes from Undo/Redo, the pair of options is
  //wrapped in a left. So, every change is accumulated but only
  //pairs wrapped in a Right will be passed on to the Undo/Redo
  //machinery, pairs wrapped in a Left will be ignored by Undo/Redo.
  private[nb] def accumPair[A](in: A \/ A, dis: UEDis[A]): UEDis[A] = {
    def accumDis(a: A) = (dis.fold(identity, identity)._2, a.some)
    
    in.bimap(accumDis, accumDis)
  }

  /** Returns a signal function that takes items of type `A` as input
    * and fires events of type `A \/ A`, where a `Left` (`-\/(_)`) comes
    * frome undo/redo, while a `Right` (`\/-(_)`) comes from new user
    * input.
    */
  def sf[A](out: Out[UndoEdit]): SF[A,A \/ A] = {
    //takes an Out[A] (provided by an internal Var by dire) and returns an
    //Out[UEDis[A]]. Used to create a signal function via SF.connectOuts
    def toEdit(oa: Out[A]): Out[UEDis[A]] = _ match {
      case \/-((Some(o), Some(n))) ⇒ out(UndoEdit(oa(o), oa(n)))
      case _                       ⇒ IO.ioUnit
    }

    def right = id[A] map { _.right[A] }
    def left = id[A] map { _.left[A] }

    //SF[UEDis[A],A \/ A]; fired events are always wrapped in a left
    def toEditSF = connectOuts(toEdit, Some(SIL)) >=> left

    //SF[A \/ A,A \/ A]
    def lp = loop(id[A\/A].scan(ini[A])(accumPair) >=> toEditSF)
    
    (right >=> lp) ⊹ right
  }

  def managedOut(manager: UndoRedo.Manager): Out[UndoEdit] = {
    def event(ue: UndoEdit) = new UndoableEditEvent(this, ue)

    ue ⇒ IO(manager.undoableEditHappened(event (ue)))
  }
}

// vim: set ts=2 sw=2 et:
