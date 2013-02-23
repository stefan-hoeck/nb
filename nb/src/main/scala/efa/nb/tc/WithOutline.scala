package efa.nb.tc

import efa.core.Localization
import java.util.Properties
import java.util.prefs.Preferences
import org.openide.explorer.view.OutlineView
import org.openide.nodes.Node
import org.openide.util.Lookup
import scala.collection.JavaConversions._
import scala.swing.Action

abstract class WithOutline(rn: Node, lkp: Option[Lookup])
extends ExplorerMgrTc(rn, lkp) with PersistentOutline {
  import WithOutline._
  
  getActionMap.put("EnlargeAction", 
    Action(""){enlarge.unsafePerformIO}.peer)

  getActionMap.put("ReduceAction", 
    Action(""){reduce.unsafePerformIO}.peer)
}

object WithOutline {
  private[tc] val RowHeight = "RowHeight"
  private[tc] val OutlineView = "OutlineView"
  private[tc] val MaxRowHeight = 525
  private[tc] val MinRowHeight = 25
  private[tc] val Step = 10
  
  def adjustLocalization(ov: OutlineView, locs: List[Localization]): Unit =
    locs foreach {l => ov.addPropertyColumn(l.name, l.shortName, l.desc)}
}

// vim: set ts=2 sw=2 et:
