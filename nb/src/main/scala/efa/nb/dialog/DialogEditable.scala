package efa.nb.dialog

import efa.core._
import efa.nb.{VSIn, loc}
import efa.react.{Out, EET, eTrans}
import scala.swing.event.ActionEvent
import javax.swing.JDialog
import org.openide.{DialogDisplayer, DialogDescriptor, NotifyDescriptor}
import scala.swing.{Component, Button, BorderPanel}
import scalaz._, Scalaz._, effect.IO

/** Type class used to edit objects in a popup dialog */
trait DialogEditable[-A,+B] {
  import efa.react.swing.abstractButton._
  import DialogEditable._

  /** The componens used for editing */
  type Comp <: Component

  /** Creates a new component for editing
    * @param a The object to be edited
    * @param isCreate `true` if a new object is to be created
    *                 `false`otherwise (if a is an existing object that
    *                  is updated by the user
    */
  def component (a: A, isCreate: Boolean): IO[Comp]

  def signalIn (c: Comp): VSIn[B]

  def name (a: A): String = a.toString

  def editTitle (a: A): String = loc editTitle name(a)

  def newTitle (a: A): String = loc newTitle name(a)

  final def editDialog (
    title: String,
    a: A,
    isCreate: Boolean): IO[Option[B]] = for {
    p    ← component(a, isCreate)
    pnl  ← IO(new BorderPanel{add (p, BorderPanel.Position.North)})
    desc ← IO (new NotifyDescriptor.Confirmation(
              pnl.peer, title, NotifyDescriptor.OK_CANCEL_OPTION))
    _    ← IO(desc.createNotificationLineSupport())
    cs   ← signalIn(p) to descOut(desc) runIO ()
    b    ← IO(DialogDisplayer.getDefault.notify(desc))
    ok   = b == NotifyDescriptor.YES_OPTION ||
           b == NotifyDescriptor.OK_OPTION
    res  ← ok ? cs._2.now.map (_.toOption) | IO(None)
    _    ← cs._1.toList foldMap (_.disconnect) //clean up events
  } yield res

  final def edit (a: A): IO[Option[B]] = editDialog(editTitle(a), a, false)

  final def create (a: A): IO[Option[B]] = editDialog(newTitle(a), a, true)

  lazy val editTrans: EET[A,B] = eTrans.id collectIO edit
}

object DialogEditable {

  def apply[A:Show,B,C <: Component](c: (A,Boolean) ⇒ C)(in: C ⇒ VSIn[B])
    : DialogEditable[A,B] = io[A,B,C]((a, b) ⇒ (IO(c(a, b))))(in)

  def io[A:Show,B,C <: Component](c: (A, Boolean) ⇒ IO[C])(in: C ⇒ VSIn[B])
    : DialogEditable[A,B] = new DialogEditable[A,B] {
    type Comp = C
    def component (a: A, isCreate: Boolean) = c(a, isCreate)
    def signalIn (c: C) = in(c)
    override def name (a: A) = Show[A] shows a
  }

  def io1[A:Show,B,C <: Component](c: A ⇒ IO[C])(in: C ⇒ VSIn[B])
    : DialogEditable[A,B] = io[A,B,C]{ (a,b) ⇒ c(a) }(in)

  def descOut[A](desc: NotifyDescriptor): Out[ValRes[A]] = {
    def valid (a: A) = IO{
      desc.setValid(true)
      desc.getNotificationLineSupport.clearMessages()
    }

    def msg (n: NonEmptyList[String]) = IO{
      desc.setValid(false)
      desc.getNotificationLineSupport.setErrorMessage(n.head)
    }

    _ fold (msg, valid)
  }
}

// vim: set ts=2 sw=2 et:
