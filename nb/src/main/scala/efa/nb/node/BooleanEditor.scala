package efa.nb.node

import dire.swing.HAlign
import java.awt.{Rectangle, Graphics}
import java.beans.PropertyEditorSupport
import javax.swing.JCheckBox
import org.openide.explorer.propertysheet.{ExPropertyEditor,InplaceEditor, PropertyEnv}

class BooleanEditor 
   extends PropertyEditorSupport
   with ExPropertyEditor
   with InplaceEditor.Factory {

  def attachEnv(env: PropertyEnv) { env.registerInplaceEditorFactory(this) }
  override def getInplaceEditor: InplaceEditor = BooleanInplace
  override def isPaintable = true
  override def paintValue(g: Graphics, r: Rectangle) {
    val c = new JCheckBox
    
    c.setSelected(getValue.asInstanceOf[Boolean])
    c.setHorizontalAlignment(HAlign.Center.v)
    c.setBounds(r)
    c.paint(g)
  }
}

private [node] object BooleanInplace extends ComponentInplaceEditor[Boolean] {
  protected val comp = new JCheckBox
  override def get = comp.isSelected
  override def set(o: Boolean) { comp.setSelected(o) }
  override def supportsTextEntry = false
}

// vim: set ts=2 sw=2 et:
