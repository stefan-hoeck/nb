package efa.nb.tc

import java.awt.{BorderLayout}
import javax.swing.JPanel
import org.openide.explorer.{ExplorerManager, ExplorerUtils}
import efa.core.Localization
import org.openide.explorer.view.OutlineView
import org.openide.nodes.Node
import org.openide.util.Lookup
import scala.language.reflectiveCalls
import scala.swing.Panel

abstract class OutlinePanel extends Panel with Lookup.Provider{
  protected def rootNode: Node

  protected def mainColumnName = efa.core.loc.name

  protected def localizations: List[Localization]

  final protected def ov = peer.ov
  
  override lazy val peer =
    new JPanel with Lookup.Provider with ExplorerManager.Provider {
      val ov = new OutlineView(mainColumnName)
      ov.setEnabled(true)
      ov.getOutline.setRootVisible(false)
      WithOutline.adjustLocalization(ov, localizations)
      private val mgr = new ExplorerManager
      private[this] val am = getActionMap
      am.put("delete", ExplorerUtils.actionDelete(mgr, true))
      private val lkp = ExplorerUtils.createLookup(mgr, am)
      mgr.setRootContext(rootNode)
      setLayout(new BorderLayout)
      add(ov, BorderLayout.CENTER)
      override def getExplorerManager = mgr
      override def getLookup = lkp
    }

  override def getLookup = peer.getLookup
}

// vim: set ts=2 sw=2 et:
