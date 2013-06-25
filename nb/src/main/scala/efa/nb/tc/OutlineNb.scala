package efa.nb.tc

import dire.{SIn, SF, DataSource}
import dire.swing.{Elem, AsSingleElem, Component}
import efa.core.Localization
import efa.nb.{lookup, PureLookup}, lookup._
import efa.io.EfaIO._
import java.awt.BorderLayout
import java.util.Properties
import javax.swing.event.{TableColumnModelListener ⇒ TCML,
                          TableColumnModelEvent,
                          ChangeEvent, ListSelectionEvent}
import javax.swing.JPanel
import org.openide.explorer.{ExplorerManager, ExplorerUtils}
import org.openide.explorer.view.OutlineView
import org.openide.nodes.Node
import org.openide.util.Lookup
import scala.collection.JavaConversions._
import scalaz._, Scalaz._, scalaz.effect.IO

final class OutlineNb(val peer: OutlineNb.Peer) {
  import OutlineNb._

  def lookup: Lookup = peer.getLookup

  def outline = peer.view.getOutline

  def selectedColumns: SIn[List[String]] = SF cachedSrc this

  private def selNames: List[String] =
    outline.getSelectedColumns.toList map outline.getColumnName

  def adjustLocalization(locs: List[Localization]): IO[Unit] = IO {
    locs foreach {l ⇒ peer.view.addPropertyColumn(l.name, l.shortName, l.desc)}
  }

  def rowHeight = IO(outline.getRowHeight)

  def rowHeightSet(rh: Int) = IO(outline.setRowHeight(rh))

  def enlarge: IO[Unit] =
    rowHeight >>= (rh ⇒ rowHeightSet((rh + Step) min MaxRowHeight))

  def reduce: IO[Unit] =
    rowHeight >>= (rh ⇒ rowHeightSet((rh - Step) max MinRowHeight))

  def readProps(prefId: String, version: String): WithPrefs[Unit] =
    withPrefs { p ⇒
      for {
        rh ← point(p.getInt(prefId + RowHeight, MinRowHeight))
        _  ← liftIO(rowHeightSet(rh))
        _  ← try {
               val ps = propsFromArray(p.getByteArray(prefix(prefId), Array()))
               peer.view.readSettings(ps, prefix(prefId))
               ldiUnit 
             } catch {
               //do nothing, happens when no settings where stored
               case e: NullPointerException ⇒ ldiUnit
               case e: Exception ⇒  warning(readError(e, prefId))
             }
      } yield ()
    }

  def writeProps(prefId: String, version: String): WithPrefs[Unit] = 
    withPrefs { p ⇒
      for {
        rh ← liftIO(rowHeight)
        _  ← point(p.putInt(prefId + RowHeight, rh))
        ps ← point(new Properties)
        _  ← point(peer.view.writeSettings(ps, prefix(prefId)))
        _  ← point(p.putByteArray(prefix(prefId), propsToArray(ps)))
      } yield ()
    }

}

object OutlineNb {
  private[tc] val RowHeight = "RowHeight"
  private[tc] val OutlineView = "OutlineView"
  private[tc] val MaxRowHeight = 525
  private[tc] val MinRowHeight = 25
  private[tc] val Step = 10

  def apply(rootNode: Node,
            localizations: List[Localization] = Nil,
            mainColumnName: String = efa.core.loc.name,
            rootVisible: Boolean = false): IO[OutlineNb] = for {
      pl  ← PureLookup()
      ov  ← IO {
              val ov = new OutlineView(mainColumnName)
              ov.setEnabled(true)
              ov.getOutline.setRootVisible(rootVisible)
              ov
            }
      res ← IO {
              val o = new OutlineNb(new Peer(ov, rootNode, pl))
              o.peer.actionMap.put("EnlargeAction", 
                efa.nb.action(""){o.enlarge.unsafePerformIO})

              o.peer.actionMap.put("ReduceAction", 
                efa.nb.action(""){o.reduce.unsafePerformIO})

              o
            }
      _   ← res adjustLocalization localizations
  } yield res

  implicit val AsElem: AsSingleElem[OutlineNb] = Elem vhFill { _.peer }

  implicit val SelectedColumnsSource: DataSource[OutlineNb,List[String]] =
    DataSource.signalSrcInpure((_: OutlineNb).selNames){
      op ⇒ out ⇒
        val li = listener(op, out)
        op.outline.getColumnModel.addColumnModelListener(li)

        _ ⇒ op.outline.getColumnModel.removeColumnModelListener(li)
      }

  implicit val Comp: Component[OutlineNb] = new Component[OutlineNb] {
    def peer(c: OutlineNb) = c.peer
  }

  private def listener(op: OutlineNb, out: List[String] ⇒ Unit) =
    new TCML {
      def columnAdded(e: TableColumnModelEvent) {}
      def columnRemoved(e: TableColumnModelEvent) {}
      def columnMoved(e: TableColumnModelEvent) {}
      def columnMarginChanged(e: ChangeEvent) {}
      def columnSelectionChanged(e: ListSelectionEvent) { out(op.selNames) }
    }

  final class Peer private[tc](
      val view: OutlineView,
      val rootNode: Node,
      val pureLkp: PureLookup)
    extends JPanel with ExplorerManager.Provider with Lookup.Provider {

    private val mgr = new ExplorerManager

    val actionMap = getActionMap

    override lazy val getLookup =
      ExplorerUtils.createLookup(mgr, actionMap) ⊹ pureLkp.l

    actionMap.put("delete", ExplorerUtils.actionDelete(mgr, true))


    mgr.setRootContext(rootNode)
    setLayout(new BorderLayout)
    add(view, BorderLayout.CENTER)

    override def getExplorerManager = mgr
  }

  import java.io.{ByteArrayOutputStream, ByteArrayInputStream}

  private def propsToArray(ps: Properties): Array[Byte] = {
    val os = new ByteArrayOutputStream

    ps.store(os, "")
    os.close()

    os.toByteArray
  }

  private def propsFromArray (bs: Array[Byte]): Properties = {
    val in = new ByteArrayInputStream(bs)
    val ps = new Properties

    ps.load(in)
    in.close()

    ps
  }

  private def prefix(id: String) = id + OutlineView

  private def readError(e: Exception, prefId: String) = 
    s"Error when reading props for $prefId: $e"
}

trait OutlineTc[A] extends AsTc[A] {
  import OutlineNb.{RowHeight, MinRowHeight}

  def outlineNb(a: A): OutlineNb

  private def ov(a: A) = outlineNb(a).peer.view

  override def peer(a: A): javax.swing.JComponent = outlineNb(a).peer

  override def explorerMgr(a: A) =
    outlineNb(a).peer.getExplorerManager.some

  override def lookup(a: A) = outlineNb(a).peer.getLookup

  override protected def readProps(a: A) =
    outlineNb(a) readProps (preferredId, version)

  override protected def writeProps(a: A) =
    outlineNb(a) writeProps (preferredId, version)
}

// vim: set ts=2 sw=2 et:
