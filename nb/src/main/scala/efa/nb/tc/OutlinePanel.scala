package efa.nb.tc

import dire.swing.{Elem, AsSingleElem}
import efa.core.Localization
import java.awt.BorderLayout
import javax.swing.JPanel
import org.openide.explorer.{ExplorerManager, ExplorerUtils}
import org.openide.explorer.view.OutlineView
import org.openide.nodes.Node
import org.openide.util.Lookup
import scalaz.effect.IO

case class OutlinePanel(peer: OutlinePanel.OutlinePanelPeer)
  extends Lookup.Provider {

  def outlineNb: OutlineNb = peer.outlineNb

  def outlineView: OutlineView = outlineNb.peer

  def outline = outlineView.getOutline

  def getLookup = peer.getLookup
}

object OutlinePanel {

  def apply(rootNode: Node,
            localizations: List[Localization] = Nil,
            mainColumnName: String = efa.core.loc.name,
            rootVisible: Boolean = false): IO[OutlinePanel] = for {
      onb ← OutlineNb(localizations, mainColumnName, rootVisible)
      p   ← IO(new OutlinePanelPeer(onb, rootNode))
    } yield OutlinePanel(p)

  implicit val OutlinePanelElem: AsSingleElem[OutlinePanel] =
    Elem vhFill { _.peer }

  final class OutlinePanelPeer private[tc](
      val outlineNb: OutlineNb,
      rootNode: Node)
    extends JPanel with ExplorerManager.Provider with Lookup.Provider {

    private val mgr = new ExplorerManager
    val actionMap = getActionMap
    override lazy val getLookup = ExplorerUtils.createLookup(mgr, actionMap)
    actionMap.put("delete", ExplorerUtils.actionDelete(mgr, true))
    mgr.setRootContext(rootNode)
    setLayout(new BorderLayout)
    add(outlineNb.peer, BorderLayout.CENTER)

    override def getExplorerManager = mgr
  }
}

// vim: set ts=2 sw=2 et:
