package efa.nb.actions

import javax.swing.Action
import scalaz.effect.IO

trait SignalEnabledAction extends Action {
//  protected def enabledIn: SIn[Boolean]
//
//  enabledIn to (b ⇒ IO(setEnabled(b))) runIO () unsafePerformIO
}

// vim: set ts=2 sw=2 et:
