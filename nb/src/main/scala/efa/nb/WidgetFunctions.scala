package efa.nb

import dire.{SF, Out, SIn}
import dire.swing._
import efa.core._, Efa._, Validators.dummy
import scalaz._, Scalaz._

trait WidgetFunctions
  extends dire.ValidationFunctions
  with dire.ValidationInstances {

  def booleanIn(in: SIn[String],
                v: EndoVal[Boolean] = dummy[Boolean]): VSIn[Boolean] =
    readIn(in, v)

  def doubleIn(in: SIn[String],
               v: EndoVal[Double] = dummy[Double]): VSIn[Double] =
    readIn(in, v)

  def getSet[A,B,C](get: A ⇒ C)(set: (A,C) ⇒ ValSt[B], in: SfV[C,C])
    : VStSF[A,B] = {
    import scalaz.Validation.FlatMap._

    SF.id[A].upon(in ∙ get){ (a,vi) ⇒ vi flatMap (set(a,_)) }
  }

  def intIn(in: SIn[String], v: EndoVal[Int] = dummy[Int]): VSIn[Int] =
    readIn(in, v)

  def lensed[A,B](in: SF[B,B])(l: A @> B): VStSF[A,A] =
    lensedV(in >=> success)(l)

  def lensedV[A,B](in: SfV[B,B])(l: A @> B): VStSF[A,A] =
    in map (_ map (l := _ void)) contramap l.get

  def lensedVSt[A,B](in: VStSF[B,B])(l: A @> B): VStSF[A,A] =
    in andThen mapSt(l) contramap l.get

  def longIn(in: SIn[String], v: EndoVal[Long] = dummy[Long]): VSIn[Long] =
    readIn(in, v)

  def mapSt[A,B](l: B @> A): VStSF[ValSt[A],B] = {
    def nextSt(s: State[A,Unit]): State[B,Unit] =
      init[B] >>= (b ⇒ l := s.exec(l get b) void)

    SF.id map { _ map nextSt }
  }

  def readShow[A:Read:Show](sf: SF[String,String]): SfV[A,A] =
    sf andThen read[A] contramap { _.shows } 

  def read[A:Read]: SfV[String,A] = SF.id map Read[A].read

  def readIn[A:Read](in: SIn[String], v: EndoVal[A] = dummy[A]): VSIn[A] =
    in >=> read[A] >=> reValidate(v)
}

object Widgets extends WidgetFunctions

// vim: set ts=2 sw=2 et:
