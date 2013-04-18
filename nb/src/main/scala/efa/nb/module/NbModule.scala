package efa.nb.module

import efa.nb.PureLookup
import org.openide.modules.ModuleInstall
import scalaz.effect.IO

trait NbModule extends ModuleInstall {
  protected def restoredIO: IO[Unit] = IO.ioUnit

  protected def closeIO: IO[Boolean]

  final override def closing() = closeIO.unsafePerformIO()

  final override def restored() { restoredIO.unsafePerformIO() }
}

// vim: set ts=2 sw=2 et:
