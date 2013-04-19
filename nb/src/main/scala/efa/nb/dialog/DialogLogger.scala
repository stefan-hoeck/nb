package efa.nb.dialog

import efa.core.{Level, Log}
import efa.io.LoggerIO
import scalaz._, Scalaz._, effect.IO

object DialogLogger {
  lazy val logger: LoggerIO = LoggerIO (log(_).void, logNel(_).void)

  private def log (l: Log) = l.level match {
    case Level.Error    ⇒ Error msg l.msg
    case Level.Warning  ⇒ Information msg l.msg
    case _              ⇒ IO.ioUnit
  }

  private def logNel (ss: NonEmptyList[String]) =
    Error msg (ss.list mkString "\n")
}

// vim: set ts=2 sw=2 et:
