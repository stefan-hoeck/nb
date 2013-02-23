package efa.nb

import efa.react.{eTrans}
import org.openide.util.lookup.{AbstractLookup, InstanceContent}
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, scalacheck.ScalaCheckBinding._, effect.IO

object LookupEventsTest extends Properties("LookupEvents") {
  class Cc(override val toString: String)

  implicit val CcEqual: Equal[Cc] = Equal.equalBy(_.toString)
  implicit val CcArbitrary = Arbitrary(Gen.identifier ∘ (new Cc(_)))

  property("lookupEvents_one") = forAll {cc: Cc ⇒ 
    val res = for {
      ref ← IO newIORef List.empty[Cc]
      pl  ← PureLookup.apply
      lr  ← pl.result[Cc]
      _   ← (eTrans.in(lr) to (c ⇒ ref mod (c ::: _) void)).go
      _   ← pl + cc
      now ← ref.read
    } yield (now ≟ List(cc))

    res.unsafePerformIO
  }

  property("lookupEvents_many") = forAll {ccs: List[Cc] ⇒ 
    //Seems like unused values can be garbage collected in for comprehensions
    //We therefore must keep a strong reference to the LookupResultWrapper
    //Otherwise the test will fail at unexpected times.
    val pl = PureLookup.apply unsafePerformIO
    val wrap = pl.result[Cc] unsafePerformIO //Keep strong reference

    val res = for {
      ref ← IO newIORef List.empty[Cc]
      _   ← (eTrans.in(wrap) to (ref write _)).go
      _   ← pl ++ ccs
      now ← ref.read
    } yield (now ≟ ccs) :| "Exp: %s but was %s".format(ccs, now)

    res.unsafePerformIO
  }
}

// vim: set ts=2 sw=2 et:
