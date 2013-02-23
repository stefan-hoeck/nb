package efa.nb.node

import efa.react.Out
import java.awt.{Rectangle, Graphics}
import java.beans.PropertyEditorSupport
import org.openide.explorer.propertysheet.{ExPropertyEditor, InplaceEditor, PropertyEnv}
import scala.swing.{TextField, Label, Alignment}
import scalaz.effect.IO

class TextEditor(
  al: Alignment.Value,
  value: String,
  desc: Option[String],
  textOut: Out[String]
) extends PropertyEditorSupport
   with ExPropertyEditor
   with InplaceEditor.Factory{

  def attachEnv(env: PropertyEnv) { env.registerInplaceEditorFactory(this) }
  override def getInplaceEditor: InplaceEditor = new TextInplace(value)
  override def setAsText (s: String) {textOut(s).unsafePerformIO}
  override def getAsText = desc getOrElse ""
  override def isPaintable = true
  override def paintValue(g: Graphics, r: Rectangle): Unit = {
    val cbx = new Label(value){
      horizontalAlignment = al
    }
    cbx.peer.setBounds(r)
    cbx.peer.paint(g)
  }
}

object TextEditor {
  def read[A] (
    al: Alignment.Value, toString: A ⇒ String, desc: A ⇒ Option[String]
  ): Option[A ⇒ TextEditor] =
    Some (a ⇒ rw[A](a, al, toString, desc, _ ⇒ IO.ioUnit))

  def rw[A] (
    a: A, 
    al: Alignment.Value,
    toString: A ⇒ String,
    desc: A ⇒ Option[String],
    o: Out[String]
  ): TextEditor = new TextEditor(al, toString(a), desc(a), o)
}

private [node] class TextInplace (value: String)
   extends ComponentInplaceEditor[AnyRef] {
  protected val comp = new TextField {text = value}
  override def get = comp.text
  override def set(o: AnyRef) {}
  override def supportsTextEntry = true
}

// vim: set ts=2 sw=2 et:
