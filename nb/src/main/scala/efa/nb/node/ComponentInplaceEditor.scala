package efa.nb.node

import efa.core.Unerased
import java.awt.event.ActionListener
import java.awt.{Component => JComp}
import java.beans.PropertyEditor
import javax.swing.{JComponent, KeyStroke}
import org.openide.explorer.propertysheet.{InplaceEditor, PropertyModel,
  PropertyEnv}
import scalaz._, Scalaz._

abstract class ComponentInplaceEditor[T](implicit m: Unerased[T])
   extends InplaceEditor {  
  protected val comp: JComponent
  protected def get: T
  protected def set(t: T)
  private[this] var editor: Option[PropertyEditor] = None
  private[this] var model: Option[PropertyModel] = None
        
  override def connect(propertyEditor: PropertyEditor, env: PropertyEnv) {
    editor = Option(propertyEditor)
    reset()
  }
  override def getComponent: JComponent = comp
  override def clear() { editor = None; model = None }
  override def getValue = get.asInstanceOf[AnyRef]
  override def setValue(o: AnyRef) { set(m.clazz cast o) }
  override def reset() {
    editor flatMap (e ⇒ Option(e.getValue)) foreach { v ⇒ set(m.clazz cast v) }
  }
  override def getKeyStrokes = Array[KeyStroke]()
  override def getPropertyEditor = editor | null
  override def getPropertyModel = model | null
  override def setPropertyModel(m: PropertyModel) { model = Option(m) }
  override def isKnownComponent(c: JComp) = c == comp || (comp isAncestorOf c)
  override def addActionListener(ali: ActionListener) {}
  override def removeActionListener(ali: ActionListener) {}
}

// vim: set ts=2 sw=2 et:
