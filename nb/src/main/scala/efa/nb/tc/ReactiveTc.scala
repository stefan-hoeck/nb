package efa.nb.tc

import dire.SIn
import scalaz.effect.IO
import scalaz.syntax.monad._

abstract class ReactiveTc[A](sin: SIn[A]) extends EfaTc {
  private[this] var kill: IO[Unit] = IO.ioUnit

  override protected def cleanup = kill >> IO(kill = IO.ioUnit)

  override protected def initialize = for {
    k ← efa.nb.NbSystem forever sin
    _ ← IO(kill = k)
  } yield ()
}

// vim: set ts=2 sw=2 et:
