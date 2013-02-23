package efa.nb.controller

import efa.core.ValSt
import efa.nb.UndoEdit
import efa.react._
import scalaz._, Scalaz._, effect._

case class UI[A] (
  actual: IORef[A],
  in: Source[ValSt[A]],
  undos: IORef[List[UndoEdit]],
  redos: IORef[List[UndoEdit]]
) {
  def get: IO[A] = actual.read

  def set (a: A): IO[Unit] = in fire put(a).success

  def mod (f: A ⇒ A): IO[Unit] = in fire modify(f).success

  def fail (s: String): IO[Unit] = in fire s.failNel

  def undoOut (e: UndoEdit): IO[Unit] =
    undos mod (e :: _) void

  def undo: IO[Unit] = for {
    us ← undos.read
    _  ← us.headOption map (u ⇒ for {
           _ ← u.un
           _ ← undos mod (_.tail)
           _ ← redos mod (u :: _)
         } yield ()) orZero
  } yield ()

  def redo: IO[Unit] = for {
    rs ← redos.read
    _  ← rs.headOption map (r ⇒ for {
           _ ← r.re
           _ ← redos mod (_.tail)
           _ ← undos mod (r :: _)
         } yield ()) orZero
  } yield ()

  def stTrans: SET[A,ValSt[A]] =
    (sTrans.id[A] to display) >|> eTrans.inIO(IO(in))

  def display (a: A): IO[Unit] = actual write a
}

object UI {
  import StateTrans._

  def apply[A] (a: A): IO[UI[A]] = for {
    act ← IO newIORef a
    src ← Events.src[ValSt[A]]
    un  ← IO newIORef List[UndoEdit]()
    re  ← IO newIORef List[UndoEdit]()
  } yield UI(act, src, un, re)

  def basicIn[A] (a: A): IO[UI[A]] = for {
    ui ← UI(a)
    _  ← StateTrans.basicIn(ui.stTrans)(IO(a)) go
  } yield ui

  def undoIn[A] (a: A): IO[UI[A]] = for {
    ui ← UI(a)
    _  ← StateTrans.undoIn(ui.stTrans, ui.undoOut)(IO(a)) go
  } yield ui
}

// vim: set ts=2 sw=2 et:
