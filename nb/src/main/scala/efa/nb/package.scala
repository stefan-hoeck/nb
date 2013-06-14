package efa

import dire.SF
import dire.control.ReactiveSystem
import efa.core.{ValRes, Service, ValSt}
import efa.nb.spi.{NbLoc, TcPreferences}
import javax.swing.{Action, AbstractAction}
import scalaz.State

package object nb {
  type StSF[A,B] = SF[A,State[B,Unit]]

  type VStSF[A,B] = SF[A,ValSt[B]]

  lazy val loc = Service.unique[NbLoc](NbLoc)

  private[nb] lazy val pref = Service.unique[TcPreferences](TcPreferences)

  lazy val NbSystem = ReactiveSystem(reactiveLog).unsafePerformIO()

  def action(name: String)(run: () ⇒ Unit): Action = new AbstractAction(name) {
    override def actionPerformed(e: java.awt.event.ActionEvent) { run() }
  }

  private def reactiveLog(s: String) = pref.tcLogger flatMap (_ debug s)
}

// vim: set ts=2 sw=2 et:
