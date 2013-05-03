package efa.nb.dialog

import efa.core._
import efa.nb.{loc, NbSystem}
import dire._, dire.swing.{SwingStrategy, swingSink}
import javax.swing.JDialog
import org.openide.{DialogDisplayer, DialogDescriptor, NotifyDescriptor}
import scalaz._, Scalaz._, effect.IO

/** Type class used to edit objects in a popup dialog */
trait DialogEditable[-A,+B] {
  import DialogEditable._

  /** Creates a new component for editing
    * @param a The object to be edited
    * @param isCreate `true` if a new object is to be created
    *                 `false`otherwise (if a is an existing object that
    *                  is updated by the user
    */
  def info(a: A, isCreate: Boolean): DEInfo[B]

  def name(a: A): String = a.toString

  def editTitle(a: A): String = loc editTitle name(a)

  def newTitle(a: A): String = loc newTitle name(a)

  final def editDialog (
    title: String,
    a: A,
    isCreate: Boolean): IO[Option[B]] = for {
    p      ← info(a, isCreate)
    (e, s) = p
    pnl    ← e.panel
    desc   ← IO (new NotifyDescriptor.Confirmation(
                 pnl.peer, title, NotifyDescriptor.OK_CANCEL_OPTION))
    _      ← IO(desc.createNotificationLineSupport())
    ref    ← IO newIORef "".failureNel[B]
    sf     = s to descSink(desc) syncTo { ref write _ }
    kill   ← NbSystem forever sf
    b      ← IO(DialogDisplayer.getDefault.notify(desc))
    ok     = b == NotifyDescriptor.YES_OPTION ||
             b == NotifyDescriptor.OK_OPTION
    _      ← kill
    last   ← ref.read
    res    ← ok ? ref.read.map { _.toOption } | IO(None)
  } yield res

  final def edit(a: A): IO[Option[B]] = editDialog(editTitle(a), a, false)

  final def create(a: A): IO[Option[B]] = editDialog(newTitle(a), a, true)

  lazy val sf: SF[A,B] = SF sfIO (edit, SwingStrategy) collectO identity
}

object DialogEditable {
  def io[A:Show,B](inf: (A, Boolean) ⇒ DEInfo[B]): DialogEditable[A,B] =
    new DialogEditable[A,B] {
      def info(a: A, isCreate: Boolean) = inf(a, isCreate)
      override def name (a: A) = Show[A] shows a
    }

  def io1[A:Show,B](inf: A ⇒ DEInfo[B]): DialogEditable[A,B] =
    io[A,B] { (a,_) ⇒ inf(a) }

  def descSink[A](desc: NotifyDescriptor): DataSink[ValRes[A]] = {
    def valid (a: A) = IO{
      desc.setValid(true)
      desc.getNotificationLineSupport.clearMessages()
    }

    def msg (n: NonEmptyList[String]) = IO{
      desc.setValid(false)
      desc.getNotificationLineSupport.setErrorMessage(n.head)
    }

    swingSink(_ fold (msg, valid))
  }
}

// vim: set ts=2 sw=2 et:
