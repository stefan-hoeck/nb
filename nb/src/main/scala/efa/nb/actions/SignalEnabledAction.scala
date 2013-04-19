package efa.nb.actions

import efa.nb.NbSystem.forever
import dire.SIn
import javax.swing.Action
import scalaz.effect.IO

trait SignalEnabledAction extends Action {
  protected def enabledIn: SIn[Boolean]

  forever(enabledIn syncTo { b ⇒ IO(setEnabled(b)) }).unsafePerformIO()
}

// vim: set ts=2 sw=2 et:
