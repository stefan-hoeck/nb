package efa.nb

import efa.core.syntax.lookup._
import org.openide.util.Lookup
import org.openide.util.lookup.{InstanceContent, AbstractLookup}
import scalaz._, Scalaz._, effect.IO

class PureLookup private (ic: InstanceContent) {

  val l: Lookup = new AbstractLookup(ic)

  def add[A] (a: A): IO[Unit] = this + a

  def all[A:Manifest]: IO[List[A]] = l.all[A]

  def clear[A:Manifest]: IO[Unit] = all[A] >>= --

  def head[A:Manifest]: IO[Option[A]] = l.head[A]

  def mod[A:Manifest] (f: A ⇒ A): IO[Unit] =
    head[A] >>= (_ map (a ⇒ update(a, f(a))) orZero)

  def remove[A] (a: A): IO[Unit] = this - a

  def result[A](implicit m: Manifest[A]): IO[LookupResultWrapper[A]] = 
    LookupResultWrapper(l)

  def set[A:Manifest] (a: A): IO[Unit] = set (List(a))

  def set[A:Manifest] (as: List[A]): IO[Unit] = clear >> ++(as)

  def update[A] (o: A, n: A): IO[Unit] = remove(o) >> add(n)

  def -[A] (a: A): IO[Unit] = IO(ic remove a)

  def --[A] (s: List[A]): IO[Unit] = s foldMap remove

  def +[A] (a: A): IO[Unit] = IO(ic add a)

  def ++[A] (s: List[A]): IO[Unit] = s foldMap add
}

object PureLookup {
  def apply: IO[PureLookup] = IO(new PureLookup (new InstanceContent))
}

// vim: set ts=2 sw=2 et:
