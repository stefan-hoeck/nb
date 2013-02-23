package efa.nb.actions

import java.awt.event.ActionEvent
import org.openide.util.HelpCtx
import org.openide.util.actions.SystemAction
import scalaz.effect.IO

abstract class NbSystemAction (val getName: String) extends SystemAction {
  override def getHelpCtx = HelpCtx.DEFAULT_HELP
  override final def actionPerformed(e: ActionEvent) {run.unsafePerformIO}

  protected def run: IO[Unit]
}

// vim: set ts=2 sw=2 et:
