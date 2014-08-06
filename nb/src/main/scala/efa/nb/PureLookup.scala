package efa.nb

import dire.{Out, DataSource, SIn, SF}
import efa.core.Unerased
import efa.core.syntax.{lookup ⇒ lkp}
import org.openide.util.{Lookup, LookupListener, LookupEvent}
import org.openide.util.lookup.{InstanceContent, AbstractLookup}
import scala.reflect.runtime.universe.TypeTag
import scalaz._, Scalaz._, effect.IO

/** Wrapper for a Netbeans `Lookup` to which data can
  * be added as well.
  *
  * Methods in this class are thread-safe (at least as
  * thread safe as `Lookup` and `InstanceContent`)
  */
class PureLookup private(ic: InstanceContent) {
  import PureLookup._

  val l: Lookup = new AbstractLookup(ic)

  def add[A](a: A): IO[Unit] = this + a

  def all[A:Unerased]: IO[List[A]] = l.all[A]

  def clear[A:Unerased]: IO[Unit] = all[A] >>= --

  def head[A:Unerased]: IO[Option[A]] = l.head[A]

  def mod[A:Unerased](f: A ⇒ A): IO[Unit] =
    head[A] >>= (_ map (a ⇒ update(a, f(a))) orZero)

  def remove[A](a: A): IO[Unit] = this - a

  def results[A:Unerased]: SIn[List[A]] = {
    import lookup.LookupOps

    l.results[A]
  }

  def resultsCached[A:Unerased:TypeTag]: SIn[List[A]] = 
    SF.cached(results[A], this)

  def set[A:Unerased](a: A): IO[Unit] = set (List(a))

  def set[A:Unerased](as: List[A]): IO[Unit] = clear >> ++(as)

  def update[A](o: A, n: A): IO[Unit] = remove(o) >> add(n)

  def -[A](a: A): IO[Unit] = IO(ic remove a)

  def --[A](s: List[A]): IO[Unit] = s foldMap remove

  def +[A](a: A): IO[Unit] = IO(ic add a)

  def ++[A](s: List[A]): IO[Unit] = s foldMap add
}

object PureLookup {
  def apply(): IO[PureLookup] = IO(new PureLookup (new InstanceContent))
}

// vim: set ts=2 sw=2 et:
