package efa.nb.spi

import efa.core.Default
import efa.io.LoggerIO
import scalaz.effect.IO

trait TcPreferences {
  def tcLogger: IO[LoggerIO]
}

object TcPreferences extends TcPreferences {
  implicit val defInst: Default[TcPreferences] = Default.default(this)

  def tcLogger = IO(LoggerIO.consoleLogger)
}

// vim: set ts=2 sw=2 et:
