package efa

import efa.core.ValRes
import efa.nb.spi.{NbLoc, TcPreferences}
import efa.react.{SIn, EET, SET}
import scalaz.State

package object nb {

  lazy val loc = efa.core.Service.unique[NbLoc] (NbLoc)

  private[nb] lazy val pref =
    efa.core.Service.unique[TcPreferences] (TcPreferences)

  type VSIn[+A] = SIn[ValRes[A]]
  type ValEET[A,B] = EET[A,ValRes[B]]
  type ValSET[A,B] = SET[A,ValRes[B]]
  type StSET[A,B] = SET[A,State[B,Unit]]
  type VSET[A,B] = SET[A,ValRes[State[B,Unit]]]
}

// vim: set ts=2 sw=2 et:
