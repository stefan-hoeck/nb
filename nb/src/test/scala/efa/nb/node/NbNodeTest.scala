package efa.nb.node

import dire._, SF.once
import efa.core.{Validators, ValRes}
import efa.core.syntax.lookup._
import org.openide.cookies.EditCookie
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO
import scalacheck.ScalaCheckBinding._

object NbNodeTest
  extends Properties("NbNode") 
  with dire.util.TestFunctions {
  import NbNode._

  implicit val InputEqual: Equal[Input] = Equal.equalA

  property("contextRoots") = forAll { ss: List[String] ⇒
    outTestIO(ss, contextRoots, _.getCrs, ss)
  }

  property("name") = forAll { s: String ⇒
    outTest(s, NbNode.name[String](identity), _.getDisplayName, s)
  }

  property("desc") = Prop.forAll { s: String ⇒
    outTest(s, desc[String] (identity), _.getShortDescription, s)
  }

  case class Cc (s: String)

  val dummy = Cc("dummy")
  implicit val CcEqual: Equal[Cc] = Equal.equalA

  val ccGen = Gen.identifier map (Cc.apply)
  implicit val CcArbitrary: Arbitrary[Cc] = Arbitrary(ccGen)

  property("cookie") = forAll { cc: Cc ⇒
    outTestIO(cc, cookie[Cc], _.getLookup.all[Cc], List(cc))
  }

  property("cookies") = forAll(Gen listOf ccGen map (_.distinct)) { cc ⇒
    outTestIO(cc, cookies[Cc], _.getLookup.all[Cc], cc)
  }

  property("cookieOption") = forAll { cc: Option[Cc] ⇒
    outTestIO(cc, cookieOption[Cc], _.getLookup.all[Cc], cc.toList)
  }

  val nameVal = Validators maxStringLength 20

  property ("renameD") = forAll { s: String ⇒ 
    simulate(List(Rename(s).e), true)(eventSf(dummy)) ≟ 
      List(Renamed(nameVal(s).validation).i)
  }

  property ("destroy") = forAll { cc: Cc ⇒ 
    simulate(List(Destroy.e), true)(eventSf(cc)) ≟ 
      List(Destroyed(cc).i)
  }

  property ("edit") = forAll { cc: Cc ⇒ 
    simulate(List(Edit.e), true)(eventSf(cc)) ≟ 
      List(Edited(cc).i)
  }
//
//  property ("destroy") = forAll { cc: Cc ⇒ 
//    val res = for {
//      r   ← IO newIORef none[Cc]
//      n   ← NbNode.apply
//      _   ← destroy[Cc] set n to (r write _.some) runIO Signal.newVal(cc)
//      _   ← IO(n.destroy)
//      res ← r.read
//    } yield res ≟ cc.some
//
//    res.unsafePerformIO
//  }
//
//  property ("edit") = forAll { cc: Cc ⇒ 
//    val res = for {
//      r   ← IO newIORef none[Cc]
//      n   ← NbNode.apply
//      _   ← edit[Cc] set n to (r write _.some) runIO Signal.newVal(cc)
//      _   = e foreach (_.edit())
//      res ← r.read
//    } yield res ≟ cc.some
//
//    res.unsafePerformIO
//  }

  def testSF[A,B](no: NodeOut[A,B], n: NbNode)(ini: A): SIn[B] =
    SF.const(ini) >=> no.sfST(n, Sequential)

  def outTest[A,B:Equal,C](
    a: A, out: NodeOut[A,C], get: NbNode ⇒ B, must: B
  ): Prop = outTestIO (a, out, get map (IO(_)), must)

  def outTestIO[A,B:Equal,C](
    a: A, out: NodeOut[A,C], get: NbNode ⇒ IO[B], must: B
  ): Prop = {
    def msg (b: B) = s"Failure for input $a: Expected $must but was $b"

    def res = for {
      n ← NbNode.apply
      _ = runN(testSF(out, n)(a), 0)
      b ← get(n)
    } yield (b ≟ must) :| msg(b)

    res.unsafePerformIO
  }

  sealed trait Event { def e: Event = this }
  case class Rename(s: String) extends Event
  case object Destroy extends Event
  case object Edit extends Event

  sealed trait Input { def i: Input = this }
  case class Renamed(s: ValRes[String]) extends Input
  case class Destroyed(cc: Cc) extends Input
  case class Edited(cc: Cc) extends Input

  private def eventSf(cc: Cc)(o: Out[Unit]): IO[SF[Event,Input]] = for {
    n  ← NbNode()
    sf ← IO {
           def onE(e: Event): IO[Unit] = e match {
             case Rename(s) ⇒ IO(n.setName(s))
             case Destroy   ⇒ IO(n.destroy())
             case Edit      ⇒ n.getLookup.head[EditCookie] >>= {
                                _.cata(e ⇒ IO(e.edit()), IO.ioUnit) }
           }

           val nodeOut: NodeOut[Unit,Input] = 
             (edit[Cc] map { Edited(_).i } contramap { _: Unit ⇒ cc }) ⊹
             (renameV map { Renamed(_).i } contramap { _ ⇒ nameVal }) ⊹
             (destroy[Cc] map { Destroyed(_).i } contramap { _ ⇒ cc }) ⊹
             NodeOut[Unit,Input]((_,_) ⇒ o)
           
           (SF.id[Event] syncTo onE) >> (once(()) >=> nodeOut.sf(n))
         }
  } yield sf
}

// vim: set ts=2 sw=2 et:
