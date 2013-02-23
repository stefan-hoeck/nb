package efa.nb.tc

import efa.react.{AsEvents, eTrans, EIn, SIn}
import collection.JavaConversions._
import org.openide.explorer.view.OutlineView
import javax.swing.event.{TableColumnModelListener ⇒ TCML,
                          TableColumnModelEvent,
                          ChangeEvent, ListSelectionEvent}
import scalaz._, Scalaz._, effect.IO


trait OutlineSignals {
  implicit lazy val OVColumnNameEvents =
    AsEvents.unit(addTcml)(removeTcml)

  def selectedColumnNamesE (o: OutlineView): EIn[List[String]] = eTrans in o

  def selectedColumnNamesS (o: OutlineView): SIn[List[String]] = 
    selectedColumnNamesE(o) holdIO (IO(selectedColumnNames(o)))

  private def removeTcml (o: OutlineView, l: TCML) =
    o.getOutline.getColumnModel.removeColumnModelListener(l)

  private def addTcml (o: OutlineView, f: List[String] ⇒ Unit): TCML = {
    val l = new TCML {
      def columnAdded(e: TableColumnModelEvent) {}
      def columnRemoved(e: TableColumnModelEvent) {}
      def columnMoved(e: TableColumnModelEvent) {}
      def columnMarginChanged(e: ChangeEvent) {}
      def columnSelectionChanged(e: ListSelectionEvent) {
        f(selectedColumnNames(o))
      }
    }

    o.getOutline.getColumnModel.addColumnModelListener(l)

    l
  }

  private def selectedColumnNames (o: OutlineView): List[String] = {
    def selCol(i: Int) = o.getOutline.getColumnName(i)

    o.getOutline.getSelectedColumns.toList map selCol
  }
}

object OutlineSignals extends OutlineSignals

// vim: set ts=2 sw=2 et:
