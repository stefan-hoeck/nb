package efa.nb.node

import java.awt.{Rectangle, Graphics}
import java.beans.PropertyEditorSupport
import org.openide.explorer.propertysheet.{ExPropertyEditor,InplaceEditor, PropertyEnv}
import scala.swing.{Alignment, CheckBox}

class BooleanEditor 
   extends PropertyEditorSupport
   with ExPropertyEditor
   with InplaceEditor.Factory {

  def attachEnv(env: PropertyEnv) { env.registerInplaceEditorFactory(this) }
  override def getInplaceEditor: InplaceEditor = BooleanInplace
  override def isPaintable = true
  override def paintValue(g: Graphics, r: Rectangle) {
    val cbx = new CheckBox {
      selected = getValue.asInstanceOf[Boolean]
      horizontalAlignment = Alignment.Center
    }
    cbx.peer.setBounds(r)
    cbx.peer.paint(g)
  }
}

private [node] object BooleanInplace extends ComponentInplaceEditor[Boolean] {
  protected val comp = new CheckBox
  override def get = comp.selected
  override def set(o: Boolean) { comp.selected = o }
  override def supportsTextEntry = false
}

// vim: set ts=2 sw=2 et:
