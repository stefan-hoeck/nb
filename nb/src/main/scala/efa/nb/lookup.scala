package efa.nb

import dire.{Out, DataSource, SIn, SF}
import efa.core.syntax.lookup.{LookupOps ⇒ LOps}
import org.openide.util.{Lookup, LookupListener, LookupEvent}
import scala.reflect.runtime.universe.TypeTag
import scalaz.effect.IO

object lookup {
  implicit class LookupOps(val l: Lookup) extends AnyVal {
    def results[A:Manifest]: SIn[List[A]] = SF.src(l)(src[A])
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
        IO(r.allInstances.toList: List[A]) flatMap out unsafePerformIO
      }
    }
  }
}

// vim: set ts=2 sw=2 et:
