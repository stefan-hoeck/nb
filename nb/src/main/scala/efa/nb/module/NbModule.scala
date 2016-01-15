package efa.nb.module

import efa.nb.PureLookup
import org.openide.modules.ModuleInstall
import scalaz.effect.IO

trait NbModule extends ModuleInstall {
  protected def restoredIO: IO[Unit] = IO.ioUnit

  protected def closeIO: IO[Unit] = IO.ioUnit

  protected def closingIO: IO[Boolean] = IO(true)

  protected def validateIO: IO[Unit] = IO.ioUnit

  final override def close() = closeIO.unsafePerformIO()

  final override def closing() = closingIO.unsafePerformIO()

  final override def restored() = restoredIO.unsafePerformIO()

  final override def validate() = validateIO.unsafePerformIO()
}

// vim: set ts=2 sw=2 et:
