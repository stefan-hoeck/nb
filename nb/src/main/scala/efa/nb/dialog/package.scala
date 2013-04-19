package efa.nb

import dire.swing.Elem
import dire.validation.VSIn

package object dialog {
  type DEInfo[+A] = scalaz.effect.IO[(Elem, VSIn[A])]
}

// vim: set ts=2 sw=2 et:
