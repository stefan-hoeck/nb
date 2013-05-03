package efa

import dire.SF
import dire.control.ReactiveSystem
import efa.core.{ValRes, Service, ValSt}
import efa.nb.spi.{NbLoc, TcPreferences}
import scalaz.State

package object nb {
  type StSF[A,B] = SF[A,State[B,Unit]]

  type VStSF[A,B] = SF[A,ValSt[B]]

  lazy val loc = Service.unique[NbLoc](NbLoc)

  private[nb] lazy val pref = Service.unique[TcPreferences](TcPreferences)

  lazy val NbSystem = ReactiveSystem().unsafePerformIO()

  val swingS = Some(scalaz.concurrent.Strategy.SwingInvokeLater)
}

// vim: set ts=2 sw=2 et:
