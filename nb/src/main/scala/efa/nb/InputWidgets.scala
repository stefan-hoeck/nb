package efa.nb

import dire.{SF, Out, ValidationFunctions}
import dire.swing._
import efa.core._, Efa._
import scalaz._, Scalaz._

trait WidgetsFunctions extends ValidationFunctions {

//  def checkBox[A] (b: CheckBox)(l: A @> Boolean): VStSF[A,A] =
//    lensed(b.value >=> success)(l)
//
//  def comboBox[A,B] (b: ComboBox[B])(l: A @> B): VStSF[A,A] =
//    lensed(values(b) >=> success)(l)

  def getSet[A,B,C](get: A ⇒ C)(set: (A, C) ⇒ ValSt[B], in: SfV[C,C])
    : VStSF[A,B] =
    SF.id[A].upon(in ∙ get){(a,vi) ⇒ vi flatMap (set(a,_)) }
//
//  def intIn[A](
//    t: TextField,
//    v: EndoVal[Int] = Validators.dummy[Int]
//  )(l: A @> Int): VStSF[A,A] = textIn[A,Int](t, v = v)(l)
//
  def lensed[A,B](in: SfV[B,B])(l: A @> B): VStSF[A,A] =
    in map (_ map (l := _ void)) contramap l.get

  def lensedV[A,B] (in: VStSF[B,B])(l: A @> B): VStSF[A,A] =
    mapSt(in)(l) contramap l.get

  def mapSt[A,B,C] (in: VStSF[A,B])(l: C @> B): VStSF[A,C] = {
    def nextSt (s: State[B,Unit]): State[C,Unit] =
      init[C] >>= (c ⇒ l := s.exec(l get c)) void

    in map (_ map nextSt)
  }

//  def longIn[A](
//    t: TextField,
//    v: EndoVal[Long] = Validators.dummy[Long]
//  )(l: A @> Long): VStSF[A,A] = textIn[A,Long](t, v = v)(l)
//
//  def readVals[A:Read](
//    t: TextField,
//    f: A ⇒ String = (a: A) ⇒ a.toString
//  ): ValSET[A,A] = 
//    values(t) andThen validate(Read[A].validator) contramap f
//
//  def stringIn[A](
//    t: TextField,
//    v: EndoVal[String] = Validators.dummy[String]
//  )(l: A @> String): VStSF[A,A] = textIn[A,String](t, v = v)(l)
//
//  def textIn[A,B:Read](
//    t: TextField,
//    v: EndoVal[B] = Validators.dummy[B],
//    str: B ⇒ String = (b: B) ⇒ b.toString
//  )(l: A @> B): VStSF[A,A] =
//    lensed(readVals(t, str) andThen revalidate(v))(l)
}

object Widgets extends WidgetsFunctions

// vim: set ts=2 sw=2 et:
