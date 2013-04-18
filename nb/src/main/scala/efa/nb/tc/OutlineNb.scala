package efa.nb.tc

import dire.{SIn, SF, DataSource}
import dire.swing.{Wrapped, Elem, AsSingleElem}
import efa.core.Localization
import javax.swing.event.{TableColumnModelListener ⇒ TCML,
                          TableColumnModelEvent,
                          ChangeEvent, ListSelectionEvent}
import org.openide.explorer.view.OutlineView
import org.openide.nodes.Node
import scalaz._, Scalaz._, scalaz.effect.IO

case class OutlineNb(peer: OutlineView) extends Wrapped[OutlineView] {
  import OutlineNb._

  def outline = peer.getOutline

  def selectedColumns: SIn[List[String]] = SF cachedSrc this

  private def selNames: List[String] =
    outline.getSelectedColumns.toList map outline.getColumnName

  def rowHeight = IO (outline.getRowHeight)

  def rowHeightSet(rh: Int) = IO(outline.setRowHeight(rh))

  def enlarge: IO[Unit] =
    rowHeight >>= (rh ⇒ rowHeightSet((rh + Step) min MaxRowHeight))

  def reduce: IO[Unit] =
    rowHeight >>= (rh ⇒ rowHeightSet((rh - Step) max MinRowHeight))
}

object OutlineNb {
  private[tc] val RowHeight = "RowHeight"
  private[tc] val OutlineView = "OutlineView"
  private[tc] val MaxRowHeight = 525
  private[tc] val MinRowHeight = 25
  private[tc] val Step = 10
  
  def adjustLocalization(ov: OutlineView, locs: List[Localization]): Unit =
    locs foreach {l => ov.addPropertyColumn(l.name, l.shortName, l.desc)}

  def apply(localizations: List[Localization] = Nil,
            mainColumnName: String = efa.core.loc.name,
            rootVisible: Boolean = false): IO[OutlineNb] = IO {
    val ov = new OutlineView(mainColumnName)
    ov.setEnabled(true)
    ov.getOutline.setRootVisible(rootVisible)
    adjustLocalization(ov, localizations)

    OutlineNb(ov)
  }

  implicit val OutlineNbElem: AsSingleElem[OutlineNb] =
    Elem vhFill { _.peer }

  implicit val OutlineNbSelectedColumnsSource
    : DataSource[OutlineNb,List[String]] =
    DataSource.signalSrcInpure((_: OutlineNb).selNames){
      op ⇒ out ⇒ 
        val li = listener(op, out)
        op.outline.getColumnModel.addColumnModelListener(li)

        _ ⇒ op.outline.getColumnModel.removeColumnModelListener(li)
      }

  private def listener(op: OutlineNb, out: List[String] ⇒ Unit) =
    new TCML {
      def columnAdded(e: TableColumnModelEvent) {}
      def columnRemoved(e: TableColumnModelEvent) {}
      def columnMoved(e: TableColumnModelEvent) {}
      def columnMarginChanged(e: ChangeEvent) {}
      def columnSelectionChanged(e: ListSelectionEvent) { out(op.selNames) }
    }
}

// vim: set ts=2 sw=2 et:
