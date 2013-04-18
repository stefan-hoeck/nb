package efa

import dire.control.ReactiveSystem
import efa.core.ValRes
import efa.nb.spi.{NbLoc, TcPreferences}
import scalaz.State

package object nb {

  lazy val loc = efa.core.Service.unique[NbLoc] (NbLoc)

  private[nb] lazy val pref =
    efa.core.Service.unique[TcPreferences] (TcPreferences)

//  type StSET[A,B] = SET[A,State[B,Unit]]
//  type VSET[A,B] = SET[A,ValRes[State[B,Unit]]]

  lazy val NbSystem = ReactiveSystem().unsafePerformIO()
}

// vim: set ts=2 sw=2 et:
