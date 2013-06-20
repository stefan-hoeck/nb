package efa.nb

import dire.Out
import dire.swing.UndoEdit
import org.openide.awt.UndoRedo.Manager
import scalaz.effect.IO

trait UndoFunctions {
  def out(um: Manager): Out[UndoEdit] = ue ⇒ 
    IO(um.undoableEditHappened(ue event um))
}

object undo extends UndoFunctions

// vim: set ts=2 sw=2 et:
