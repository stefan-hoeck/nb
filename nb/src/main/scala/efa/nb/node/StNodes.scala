package efa.nb.node

import efa.core.{ValRes, EndoVal}
import scalaz._, Scalaz._

trait StNodeFunctions extends NbNodeFunctions with NbChildrenFunctions {

  def sg[A,B,C](set: (A,B) ⇒ C)(get: A ⇒ B)(o: ValOut[B,B])
    : ValOut[A,C] = o contramap get withIn ((a,vb) ⇒ vb map (set(a, _)))

  def lens[A,B] (o: ValStOut[A,A])(l: B @> A): ValStOut[B,B] =
    mapSt[B,A,B] (o ∙ l.get)(l)

  def mapSt[A,B,C] (o: ValStOut[A,B])(l: C @> B): ValStOut[A,C] = {
    def st (s: State[B,Unit]): State[C,Unit] =
      init[C] >>= (c ⇒ l := s.exec (l get c) void)

    o map (_ map st)
  }

  def valFromInput[A,B] (o: ValOut[A,B])(v: A ⇒ EndoVal[B])
    : ValOut[A,B] = o withIn ((a,vb) ⇒ vb flatMap (v(a) run _ validation))

}

// vim: set ts=2 sw=2 et:
