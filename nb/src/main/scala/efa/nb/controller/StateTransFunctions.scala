package efa.nb.controller

import efa.core.{ValRes, ValSt}
import efa.io.LoggerIO
import efa.nb.UndoEdit
import efa.react._
import scalaz._, Scalaz._, effect.IO

trait StateTransFunctions {
  type St[A] = State[A,Unit]
  type StTrans[A] = SET[A,ValSt[A]]

  def accum[A](a: A, st: ValSt[A]): A = st fold (_ ⇒ a, _ exec a)

  def basicIn[A] (s: StTrans[A])(a: IO[A]): SIn[A] =
    sTrans.loop(toSST(s))(a)

  def basicLogIn[A] (s: StTrans[A], l: LoggerIO)(a: IO[A]): SIn[A] =
    basicIn(s --> l.logValRes)(a)

  def undoIn[A] (s: StTrans[A], out: Out[UndoEdit])(a: IO[A]): SIn[A] =
    sTrans.loop(toSST(s) >=> UndoEdit.undoSST(out))(a)

  def undoLogIn[A] (
    s: StTrans[A],
    out: Out[UndoEdit],
    l: LoggerIO
  )(a: IO[A]): SIn[A] = undoIn (s --> l.logValRes, out)(a)

  def toSST[A] (s: StTrans[A]): SST[A,A] =
    eTrans.loopHold(s uponOut accum)

  def worldSST[A,B,W] (s: SET[B,ValSt[A]], wIn: SIn[W])
    (aaSST: SST[A,A], f: (A,W) ⇒ B): SST[A,A] = {
    def awSST: SST[A,W] = sTrans(_ ⇒ wIn run ())
    def abSST: SST[A,B] = aaSST ⊛ awSST apply f

    toSST(abSST >=> s)
  }

  def worldIn[A,B,W] (s: SET[B,ValSt[A]], wIn: SIn[W])
    (aaSST: SST[A,A], f: (A,W) ⇒ B, a: IO[A]): SIn[A] =
    sTrans.loop (worldSST(s, wIn)(aaSST, f))(a)
}

object StateTrans extends StateTransFunctions

// vim: set ts=2 sw=2 et:
