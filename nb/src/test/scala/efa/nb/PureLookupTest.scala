package efa.nb

import dire._
import org.openide.util.lookup.{AbstractLookup, InstanceContent}
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, scalacheck.ScalaCheckBinding._, effect.IO

object PureLookupTest extends Properties("PureLookup") {
  class Cc(override val toString: String)

  implicit val CcEqual: Equal[Cc] = Equal.equalBy(_.toString)
  implicit val CcArbitrary = Arbitrary(Gen.identifier ∘ (new Cc(_)))

  property("LookupResults") =  {
    val ccs = new collection.mutable.ListBuffer[List[Cc]]
    def sf(pl: PureLookup) =
      (SF.time map { t ⇒ new Cc(t.toString) } syncTo pl.add[Cc]) >>
      (pl.results[Cc] syncTo { xs ⇒ IO(ccs += xs) } count)

    PureLookup() >>= { pl ⇒ SF.run(sf(pl), 2, 1L)(_ >= 10) } unsafePerformIO

    (ccs.size ≟ 10) && (ccs.last.size ≟ 10)
  }
}

// vim: set ts=2 sw=2 et:
