package efa.nb.dialog

import dire.DataSink
import dire.swing.{Elem, swingSink}
import org.openide.{DialogDisplayer, NotifyDescriptor}
import NotifyDescriptor.{YES_OPTION, NO_OPTION, OK_OPTION, CANCEL_OPTION}
import scalaz.Scalaz._, scalaz.effect.IO

sealed trait Dialog[A] {
  self ⇒ 
  
  final def msg(msg: String, title: String = ""): IO[A] =
    displayMessage(msg, title)
  
  final def apply(msg: Elem, title: String = ""): IO[A] = for {
    p ← msg.panel
    b ← displayMessage(p.peer, title)
  } yield b

  final val sink: DataSink[String] = swingSink(msg(_).void)
  
  /**
   * Displays the message object in a modal dialog
   * @param message the object to be displayed.
   * @return true if the user pressed the OK, or YES-Button to exit the dialog
   */
  private[this] def displayMessage (msg: AnyRef, title: String)
  : IO[A] = for {
      desc ← descriptor(msg, title)
      rv   = DialogDisplayer.getDefault().notify(desc)
    } yield getReply(rv)

  private[dialog] def descriptor(msg: AnyRef, title: String)
  : IO[NotifyDescriptor]

  private[dialog] def getReply(n: AnyRef): A

}

/**
 * Displays a message to the user that can be confirmed (closed) by clicking
 * an OK-Button
 */
case object Message extends Dialog[Unit] {
  private[dialog] def descriptor(msg: AnyRef, title: String) = IO (
    new NotifyDescriptor.Message(msg, NotifyDescriptor.PLAIN_MESSAGE)
  )

  private[dialog] def getReply(n: AnyRef) = ()
}

/**
 * Displays some information to the user. Same as Message but with a
 * different icon.
 */
case object Information extends Dialog[Unit] {
  private[dialog] def descriptor(msg: AnyRef, title: String) = IO (
    new NotifyDescriptor.Message(msg, NotifyDescriptor.INFORMATION_MESSAGE)
  )

  private[dialog] def getReply(n: AnyRef) = ()
}

/**
 * Informs the user about an error. Same as Message but with a different icon.
 */
case object Error extends Dialog[Unit] {
  private[dialog] def descriptor(msg: AnyRef, title: String) = IO (
    new NotifyDescriptor.Message(msg, NotifyDescriptor.ERROR_MESSAGE)
  )

  private[dialog] def getReply(n: AnyRef) = ()
}

/**
 * Asks the user to confirm some action.
 */
case object Confirmation extends Dialog[ConfirmationResult] {
  private[dialog] def descriptor(msg: AnyRef, title: String) = IO (
    new NotifyDescriptor.Confirmation(msg)
  )

  private[dialog] def getReply(n: AnyRef) = ConfirmationResult fromReply n
}

/**
 * Allows the user to enter some input via a Dialog window. In this case,
 * the message is usually a scala component.
 */
case object Input extends Dialog[Boolean] {
  private[dialog] def descriptor(msg: AnyRef, title: String) = {
    import efa.nb.loc
    def tit = title.isEmpty ? loc.inputTitle | title

    IO (new NotifyDescriptor.Confirmation(
      msg, tit, NotifyDescriptor.OK_CANCEL_OPTION))
  }

  private[dialog] def getReply(n: AnyRef) = {
    n == YES_OPTION || n == OK_OPTION
  }
}

sealed trait ConfirmationResult {
  def toBool: Boolean
}

object ConfirmationResult {
  case object Yes extends ConfirmationResult {
    def toBool = true
  }

  case object No extends ConfirmationResult {
    def toBool = false
  }

  case object Cancel extends ConfirmationResult {
    def toBool = false
  }

  def fromReply(r: AnyRef): ConfirmationResult =
    if (r == YES_OPTION || r == OK_OPTION) Yes
    else if (r == NO_OPTION) No
    else Cancel
}

// vim: set ts=2 sw=2 et:
