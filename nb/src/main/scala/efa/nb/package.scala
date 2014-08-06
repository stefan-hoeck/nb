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

  lazy val loc = Service.unique[NbLoc]

  private[nb] lazy val pref = Service.unique[TcPreferences]

  lazy val NbSystem = ReactiveSystem(reactiveLog).unsafePerformIO()

  def action(name: String)(run: () ⇒ Unit): Action = new AbstractAction(name) {
    override def actionPerformed(e: java.awt.event.ActionEvent) { run() }
  }

  private final val EnableLogging = "NbSystem.loggingEnabled"

  private lazy val reactiveLog = System.getProperty(EnableLogging) match {
    case "true" ⇒ (s: String) ⇒ efa.io.LoggerIO.consoleLogger debug s
    case _      ⇒ (s: String) ⇒ scalaz.effect.IO.ioUnit
  }
}

// vim: set ts=2 sw=2 et:
