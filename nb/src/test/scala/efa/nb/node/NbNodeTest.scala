package efa.nb.node

import efa.core.Validators
import efa.core.syntax.lookup._
import efa.react._
import org.openide.cookies.EditCookie
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect.IO
import scalacheck.ScalaCheckBinding._

object NbNodeTest extends Properties("NbNode") {
  import NbNode._

  property ("contextRoots") = forAll { ss: List[String] ⇒
    outTestIO (ss, contextRoots, _.getCrs, ss)
  }

  property ("name") = forAll { s: String ⇒
    outTest (s, NbNode.name[String](identity), _.getDisplayName, s)
  }

  property ("desc") = Prop.forAll { s: String ⇒
    outTest (s, desc[String] (identity), _.getShortDescription, s)
  }

  case class Cc (s: String)
  implicit val CcEqual: Equal[Cc] = Equal.equalA

  val ccGen = Gen.identifier map (Cc.apply)
  implicit val CcArbitrary: Arbitrary[Cc] = Arbitrary(ccGen)

  property ("cookie") = forAll { cc: Cc ⇒
    outTestIO (cc, cookie[Cc], _.getLookup.all[Cc], List(cc))
  }

  property ("cookies") = forAll(Gen listOf ccGen map (_.distinct)) { cc ⇒
    outTestIO (cc, cookies[Cc], _.getLookup.all[Cc], cc)
  }

  property ("cookieOption") = forAll { cc: Option[Cc] ⇒
    outTestIO (cc, cookieOption[Cc], _.getLookup.all[Cc], cc.toList)
  }

  val nameVal = Validators maxStringLength 20

  property ("renameD") = forAll { s: String ⇒ 
    val res = for {
      r   ← IO newIORef "".failureNel[String]
      n   ← NbNode.apply
      _   ← renameV set n to (r write _) runIO Signal.newVal(nameVal)
      _   ← IO(n.setName(s))
      res ← r.read
    } yield res ≟ nameVal(s).validation

    res.unsafePerformIO
  }

  property ("destroy") = forAll { cc: Cc ⇒ 
    val res = for {
      r   ← IO newIORef none[Cc]
      n   ← NbNode.apply
      _   ← destroy[Cc] set n to (r write _.some) runIO Signal.newVal(cc)
      _   ← IO(n.destroy)
      res ← r.read
    } yield res ≟ cc.some

    res.unsafePerformIO
  }

  property ("edit") = forAll { cc: Cc ⇒ 
    val res = for {
      r   ← IO newIORef none[Cc]
      n   ← NbNode.apply
      _   ← edit[Cc] set n to (r write _.some) runIO Signal.newVal(cc)
      e   ← n.getLookup.head[EditCookie]
      _   = e foreach (_.edit())
      res ← r.read
    } yield res ≟ cc.some

    res.unsafePerformIO
  }

  private def outTest[A,B:Equal,C] (
    a: A, out: NodeOut[A,C], get: NbNode ⇒ B, must: B
  ): Prop = outTestIO (a, out, get map (IO(_)), must)

  private def outTestIO[A,B:Equal,C] (
    a: A, out: NodeOut[A,C], get: NbNode ⇒ IO[B], must: B
  ): Prop = {
    def msg (b: B) = "Failure for input %s: Expected %s but was %s" format
      (a, must, b)

    def res = for {
      n ← NbNode.apply
      _ ← out set n runIO Signal.newVal(a)
      b ← get (n)
    } yield (b ≟ must) :| msg (b)

    res.unsafePerformIO
  }
}

// vim: set ts=2 sw=2 et:
