package efa.nb

import dire._, DataSink.buffer
import org.openide.util.lookup.{AbstractLookup, InstanceContent}
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, scalacheck.ScalaCheckBinding._, effect.IO

object PureLookupTest extends Properties("PureLookup") {
  case class Cc(t: Time)

  implicit val CcEqual: Equal[Cc] = Equal.equalBy(_.t)

  property("LookupResults") =  {
    val ccs = new collection.mutable.ListBuffer[List[Cc]]

    def sf(pl: PureLookup) = (SF.time map Cc syncTo pl.add[Cc]) >>
                             (pl.results[Cc] to buffer(ccs) count)

    PureLookup() >>= { pl ⇒ SF.run(sf(pl), 2, 1L)(_ >= 10) } unsafePerformIO

    (ccs.size ≟ 10) && (ccs.last.size ≟ 10)
  }
}

// vim: set ts=2 sw=2 et:
