package efa.nb.controller

import dire._, SF.{id, const, loop}
import dire.swing.UndoEdit
import efa.core.{ValRes, ValSt}
import efa.io.LoggerIO
import efa.nb.VStSF
import scalaz._, Scalaz._, effect.IO

/** Functions for modelling complex user interfaces
  *
  * Data: We distinguish between raw data that can be stored
  * in and loaded from files and that can be edited by the
  * user, and displayed data that is calculated from raw
  * data and (theoretically only one) other signal which we
  * call the outside world.
  *
  * Undo/Redo: Undo/Redo stores changes made from raw data
  * in order to conveniently undo/redo these changes. Undo/Redo
  * must also store changes from itself, therefore we need a
  * loop. However, changes from Undo/Redo are treated differently
  * than from user input, therefore we must encode in the raw
  * data type whether the last change came from Undo/Redo or
  * from user input. This is done by means of \/. Undo/Redo is
  * wrapped in a left, user input in a right
  *
  * Input: User input is validated and comes in form of a State[A,Unit].
  * Invalid input is logged back to the user in some form, valid input
  * is collected and transformed to a function A ⇒ A \/ A.
  */
trait StateTransFunctions {
  import StateTrans.{Input}

  def fold[A](dis: A \/ A): A = dis fold (identity, identity)

  def collectRaw[A:Equal](initial: IO[A]): SF[Input[A],A \/ A] = {
    def scan(a: A) = 
      id[Input[A]].scan(a.right[A])((in,last) ⇒ last fold (in, in))

    SF.io(initial map scan).distinct
  }

  def complete[Raw:Equal,Calc,World]
    (worldIn: SIn[World],
     out: Out[UndoEdit])
    (ui: SF[Calc,ValSt[Raw]])
    (calc: (Raw,World) ⇒ Calc, ini: IO[Raw]): SIn[Raw] =
    uiLoop((uiIn(worldIn)(ui)(calc) ⊹ undoIn(out)) >=> collectRaw(ini))

  def completeIsolated[Raw:Equal](ui: SF[Raw,ValSt[Raw]], out: Out[UndoEdit])
                                 (ini: IO[Raw]): SIn[Raw] =
                                 complete(const(0), out)(ui)((r,_) ⇒ r, ini)
    
  def uiIn[Raw,Calc,World]
    (worldIn: SIn[World])
    (ui: SF[Calc,ValSt[Raw]])
    (calc: (Raw,World) ⇒ Calc): SF[Raw \/ Raw, Input[Raw]] = {
    def calcSF: SF[Raw \/ Raw,Calc] =
      (id map fold[Raw]) ⊛ worldIn.sf apply calc

    calcSF andThen ui collectO valStToInput
  }

  def uiInIsolated[Raw](ui: SF[Raw,ValSt[Raw]]): SF[Raw \/ Raw, Input[Raw]] =
    uiIn(const(0))(ui)((r,_) ⇒ r)

  def uiLoop[A](sf: SF[A \/ A, A \/ A]): SIn[A] = loop(sf) map fold[A] in
    

  def undoIn[A](out: Out[UndoEdit]): SF[A \/ A, Input[A]] =
    dire.swing.undo sf out map undoToInput[A]

  def undoToInput[A](a: A): Input[A] = _ ⇒ a.left

  def valStToInput[A](s: ValSt[A]): Option[Input[A]] = 
    s.toOption map { st ⇒ st exec _ right }
}

object StateTrans extends StateTransFunctions {
  type Input[A] = A ⇒ A \/ A
}

// vim: set ts=2 sw=2 et:
