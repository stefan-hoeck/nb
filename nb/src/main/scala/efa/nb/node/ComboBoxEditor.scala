package efa.nb.node

import java.awt.{Rectangle, Graphics}
import java.beans.PropertyEditorSupport
import org.openide.explorer.propertysheet.{ExPropertyEditor, InplaceEditor,
   PropertyEnv}
import scala.swing.{Alignment, ComboBox, Label}

class ComboBoxEditor[T](values: List[T], al: Alignment.Value)
(implicit m: Manifest[T])
   extends PropertyEditorSupport
   with ExPropertyEditor
   with InplaceEditor.Factory{

  def attachEnv(env: PropertyEnv) { env.registerInplaceEditorFactory(this) }
  override def getInplaceEditor: InplaceEditor = new ComboBoxInplace[T](values)
  override def isPaintable = true
  override def paintValue(g: Graphics, r: Rectangle) {
    val cbx = new Label (getValue.toString) {
      horizontalAlignment = al
    }
    cbx.peer.setBounds(r)
    cbx.peer.paint(g)
  }
}

private [node] class ComboBoxInplace[T](values: List[T])(implicit m: Manifest[T]) 
extends ComponentInplaceEditor[T] {
  protected val comp = new ComboBox[T] (values)
  override def get = comp.selection.item
  override def set(o: T) { comp.selection.item = o }
  override def supportsTextEntry = false
}

// vim: set ts=2 sw=2 et:
