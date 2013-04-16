package efa.nb

import dire.{Out, DataSource, SIn, SF}
import efa.core.syntax.lookup._
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
class PureLookup private (ic: InstanceContent) {
  import PureLookup._

  val l: Lookup = new AbstractLookup(ic)

  def add[A] (a: A): IO[Unit] = this + a

  def all[A:Manifest]: IO[List[A]] = l.all[A]

  def clear[A:Manifest]: IO[Unit] = all[A] >>= --

  def head[A:Manifest]: IO[Option[A]] = l.head[A]

  def mod[A:Manifest] (f: A ⇒ A): IO[Unit] =
    head[A] >>= (_ map (a ⇒ update(a, f(a))) orZero)

  def remove[A] (a: A): IO[Unit] = this - a

  def results[A:Manifest]: SIn[List[A]] = SF src this

  def resultsCached[A:Manifest:TypeTag]: SIn[List[A]] = 
    SF.cached(results[A], this)

  def set[A:Manifest] (a: A): IO[Unit] = set (List(a))

  def set[A:Manifest] (as: List[A]): IO[Unit] = clear >> ++(as)

  def update[A] (o: A, n: A): IO[Unit] = remove(o) >> add(n)

  def -[A] (a: A): IO[Unit] = IO(ic remove a)

  def --[A] (s: List[A]): IO[Unit] = s foldMap remove

  def +[A] (a: A): IO[Unit] = IO(ic add a)

  def ++[A] (s: List[A]): IO[Unit] = s foldMap add
}

object PureLookup {
  def apply(): IO[PureLookup] = IO(new PureLookup (new InstanceContent))

  implicit def ResultSource[A](implicit M: Manifest[A])
    : DataSource[PureLookup,List[A]] =
    DataSource.signalSrc[PureLookup,List[A]](_.all[A])(pl ⇒ o ⇒
      for {
        res ← IO(pl.l.lookupResult(M.runtimeClass.asInstanceOf[Class[A]]))
        li  ← IO(new LooLi(res, o))
        _   ← IO(res.addLookupListener(li))
      } yield IO(res.removeLookupListener(li))
    )

  private final class LooLi[A](val r: Lookup.Result[A], out: Out[List[A]])
    extends LookupListener {

    import scala.collection.JavaConversions._
    def resultChanged(e: LookupEvent) {
      IO(r.allInstances.toList: List[A]) >>= out unsafePerformIO
    }
  }
}

// vim: set ts=2 sw=2 et:
