package efa.nb

import dire.{SF, Out, ValidationFunctions}
import dire.swing._
import efa.core._, Efa._
import scalaz._, Scalaz._

trait WidgetsFunctions extends ValidationFunctions {

  def getSet[A,B,C](get: A ⇒ C)(set: (A,C) ⇒ ValSt[B], in: SfV[C,C])
    : VStSF[A,B] =
    SF.id[A].upon(in ∙ get){ (a,vi) ⇒ vi flatMap (set(a,_)) }

  def lensed[A,B](in: SF[B,B])(l: A @> B): VStSF[A,A] =
    lensedV(in >=> success)(l)

  def lensedV[A,B](in: SfV[B,B])(l: A @> B): VStSF[A,A] =
    in map (_ map (l := _ void)) contramap l.get

  def lensedVSt[A,B](in: VStSF[B,B])(l: A @> B): VStSF[A,A] =
    in andThen mapSt(l) contramap l.get

  def mapSt[A,B](l: B @> A): VStSF[ValSt[A],B] = {
    def nextSt(s: State[A,Unit]): State[B,Unit] =
      init[B] >>= (b ⇒ l := s.exec(l get b)) void

    SF.id map { _ map nextSt }
  }

  def readShow[A:Read:Show](sf: SF[String,String]): SfV[A,A] =
    sf map Read[A].read contramap { _.shows } 

}

object Widgets extends WidgetsFunctions

// vim: set ts=2 sw=2 et:
