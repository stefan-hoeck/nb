package efa.nb

import efa.io.LogDisIO
import java.util.prefs.Preferences
import scalaz._, Scalaz._

package object tc {
  type WithPrefs[A] = Kleisli[LogDisIO,Preferences,A]

  def withPrefs[A](p: Preferences ⇒ LogDisIO[A]): WithPrefs[A] = Kleisli(p)
}

// vim: set ts=2 sw=2 et:
