package efa.nb.dialog

import dire.DataSink
import dire.swing.{SwingStrategy, Elem}
import org.openide.{DialogDisplayer, NotifyDescriptor}
import scalaz.Scalaz._, scalaz.effect.IO

sealed trait Dialog {
  self ⇒ 
  
  final def msg(msg: String, title: String = ""): IO[Boolean] =
    displayMessage(msg, title)
  
  final def apply(msg: Elem, title: String = ""): IO[Boolean] = for {
    p ← msg.panel
    b ← displayMessage(p.peer, title)
  } yield b

  final val sink: DataSink[String] = 
    DataSink.cached(msg(_).void, self, IO.ioUnit, SwingStrategy)
  
  /**
   * Displays the message object in a modal dialog
   * @param message the object to be displayed.
   * @return true if the user pressed the OK, or YES-Button to exit the dialog
   */
  private[this] def displayMessage (msg: AnyRef, title: String)
  : IO[Boolean] = for {
      desc ← descriptor(msg, title)
      retval = DialogDisplayer.getDefault().notify(desc)
      res = retval == NotifyDescriptor.YES_OPTION ||
            retval == NotifyDescriptor.OK_OPTION
    } yield res

  private[dialog] def descriptor(msg: AnyRef, title: String)
  : IO[NotifyDescriptor]
}

/**
 * Displays a message to the user that can be confirmed (closed) by clicking
 * an OK-Button
 */
case object Message extends Dialog {
  private[dialog] def descriptor(msg: AnyRef, title: String) = IO (
    new NotifyDescriptor.Message(msg, NotifyDescriptor.PLAIN_MESSAGE)
  )
}

/**
 * Displays some information to the user. Same as Message but with a
 * different icon.
 */
case object Information extends Dialog {
  private[dialog] def descriptor(msg: AnyRef, title: String) = IO (
    new NotifyDescriptor.Message(msg, NotifyDescriptor.INFORMATION_MESSAGE)
  )
}

/**
 * Informs the user about an error. Same as Message but with a different icon.
 */
case object Error extends Dialog {
  private[dialog] def descriptor(msg: AnyRef, title: String) = IO (
    new NotifyDescriptor.Message(msg, NotifyDescriptor.ERROR_MESSAGE)
  )
}

/**
 * Asks the user to confirm some action.
 */
case object Confirmation extends Dialog {
  private[dialog] def descriptor(msg: AnyRef, title: String) = IO (
    new NotifyDescriptor.Confirmation(msg)
  )
}

/**
 * Allows the user to enter some input via a Dialog window. In this case,
 * the message is usually a scala component.
 */
case object Input extends Dialog {
  private[dialog] def descriptor(msg: AnyRef, title: String) = {
    import efa.nb.loc
    def tit = title.isEmpty ? loc.inputTitle | title

    IO (new NotifyDescriptor.Confirmation(
      msg, tit, NotifyDescriptor.OK_CANCEL_OPTION))
  }
}

// vim: set ts=2 sw=2 et:
