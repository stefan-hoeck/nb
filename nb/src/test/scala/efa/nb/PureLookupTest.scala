package efa.nb

import dire._, DataSink.buffer
import org.openide.util.lookup.{AbstractLookup, InstanceContent}
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, scalacheck.ScalaCheckBinding._, effect.IO

case class Cc(t: Time)

object PureLookupTest
  extends Properties("PureLookup")
  with dire.util.TestFunctions {

  implicit val CcEqual: Equal[Cc] = Equal.equalBy(_.t)

  property("LookupResults") =  {
    val ccs = new collection.mutable.ListBuffer[List[Cc]]

    def sf(pl: PureLookup) = (SF.time map Cc syncTo pl.add[Cc]) >>
                             (pl.results[Cc] to buffer(ccs) count)

    runUntil(SF io (PureLookup() map sf))(_ >= 10)

    val string = ccs.toList mkString "\n"
    (ccs.size ≟ 10 :| "ccs size")
  }
}

// vim: set ts=2 sw=2 et:
