package efa.nb

import efa.react._
import org.openide.util.{Lookup, LookupListener, LookupEvent}
import scalaz._, Scalaz._, effect.IO

/**
 * Lookups are stupid: We need to keep strong references both for LookupListeners
 * as well as Lookup.Results otherwise they get garbage collected. That's why we
 * use this helper class. Keep a strong reference to it and it provides
 * an implementation of type class AsEvents.
 *
 * CAUTION: Lookup.Results are garbage collected if not stongly referenced!!
 */
class LookupResultWrapper[A] private (
  private[nb] val res: Lookup.Result[A],
  private[this] val src: Source[List[A]],
  private[this] val lst: LookupListener
) {
  def --> (o: Out[List[A]]): IOC[Unit] = src --> o
}

object LookupResultWrapper {
  def apply[A](l: Lookup)(implicit M: Manifest[A])
    : IO[LookupResultWrapper[A]] = for {
    src ← Events.src[List[A]]
    res ← IO(l.lookupResult(M.runtimeClass.asInstanceOf[Class[A]]))
    lis ← listener(src, res)
    _   ← IO(res.addLookupListener(lis))
    wrp ← IO(new LookupResultWrapper(res, src, lis))
  } yield wrp

  private[this] def listener[A](
    src: Source[List[A]],
    res: Lookup.Result[A]
  ) = IO (
    new LookupListener {
      import scala.collection.JavaConversions._
      private def get: List[A] = res.allInstances.toList
      def resultChanged (e: LookupEvent) {
        IO(get) >>= (src fire _) unsafePerformIO
      }
    }
  )

  implicit def LookupResultWrapperAsEvents[A]
    : AsEvents[LookupResultWrapper[A],List[A]] =
    new AsEvents[LookupResultWrapper[A],List[A]] {
      def --> (l: LookupResultWrapper[A], o: Out[List[A]]) = l --> o
    }
}

// vim: set ts=2 sw=2 et:
