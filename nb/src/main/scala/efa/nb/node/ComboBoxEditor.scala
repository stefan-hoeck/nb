package efa.nb.node

import dire.swing.HAlign
import java.awt.{Rectangle, Graphics}
import java.beans.PropertyEditorSupport
import javax.swing.{JComboBox, JLabel}
import org.openide.explorer.propertysheet.{ExPropertyEditor, InplaceEditor,
   PropertyEnv}

class ComboBoxEditor[T](values: List[T], al: HAlign)
                       (implicit m: Manifest[T])
   extends PropertyEditorSupport
   with ExPropertyEditor
   with InplaceEditor.Factory{

  def attachEnv(env: PropertyEnv) { env.registerInplaceEditorFactory(this) }
  override def getInplaceEditor: InplaceEditor = new ComboBoxInplace[T](values)
  override def isPaintable = true
  override def paintValue(g: Graphics, r: Rectangle) {
    val l = new JLabel (getValue.toString)

    l.setHorizontalAlignment(al.v)
    l.setBounds(r)
    l.paint(g)
  }
}

private [node] class ComboBoxInplace[T](values: List[T])(implicit m: Manifest[T]) 
    extends ComponentInplaceEditor[T] {
  import scala.collection.JavaConversions._

  protected val comp = {
    val v = new java.util.Vector[T](values.size)

    values foreach v.add

    new JComboBox[T](v)
  }

  override def get = comp.getItemAt(comp.getSelectedIndex)
  override def set(o: T) { comp.setSelectedItem(o) }
  override def supportsTextEntry = false
}

// vim: set ts=2 sw=2 et:
