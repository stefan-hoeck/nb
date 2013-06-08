package efa.nb.node

import scalaz.effect.IO

trait Editable[-A,+B] {
  def apply(a: A, isNew: Boolean): IO[Option[B]]

  def name(a: A): String

  final def edit(a: A): IO[Option[B]] = apply(a, false)

  final def create(a: A): IO[Option[B]] = apply(a, true)
}

object Editable {
  def apply[A,B](implicit v: Editable[A,B]): Editable[A,B] = v
}

// vim: set ts=2 sw=2 et:
