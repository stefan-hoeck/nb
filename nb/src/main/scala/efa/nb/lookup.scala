package efa.nb

import dire.{Out, DataSource, SIn, SF}
import efa.core.syntax.{LookupOps ⇒ LOps}
import org.openide.util.{Lookup, LookupListener, LookupEvent}
import org.openide.util.lookup.ProxyLookup
import scalaz.effect.IO
import scalaz.syntax.monad._

object lookup {
  implicit class LookupOps(val l: Lookup) extends AnyVal {
    def results[A:Manifest]: SIn[List[A]] = SF.src(l)(src[A])
  }

  implicit val LookupMonoid = new scalaz.Monoid[Lookup] {
    val zero = Lookup.EMPTY
    def append(a: Lookup, b: ⇒ Lookup): Lookup = new ProxyLookup(a, b)
  }

  private def lops(l: Lookup): LOps = l

  private def src[A](implicit M: Manifest[A]): DataSource[Lookup,List[A]] =
    DataSource.signalSrc[Lookup,List[A]](lops(_).all[A])(l ⇒ o ⇒
      for {
        res ← IO(l.lookupResult(M.runtimeClass.asInstanceOf[Class[A]]))
        li  ← looli(res, o)
        _   ← IO(res.addLookupListener(li))
      } yield IO(res.removeLookupListener(li))
    )

  private def looli[A](r: Lookup.Result[A], out: Out[List[A]]) = IO {
    new LookupListener {
      import scala.collection.JavaConversions._
      def resultChanged(e: LookupEvent) {
        IO(r.allInstances.toList: List[A]) flatMap (as ⇒ out(as)) unsafePerformIO
      }
    }
  }
}

// vim: set ts=2 sw=2 et:
