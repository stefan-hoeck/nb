package efa.nb.tc

import efa.core.Localization
import java.util.Properties
import java.util.prefs.Preferences
import org.openide.explorer.view.OutlineView
import org.openide.nodes.Node
import org.openide.util.Lookup
import scala.collection.JavaConversions._
import scala.swing.Action

abstract class WithOutline(
    final override protected val outlineNb: OutlineNb,
    rn: Node,
    lkp: Option[Lookup])
  extends ExplorerMgrTc(rn, lkp) with PersistentOutline {
  getActionMap.put("EnlargeAction", 
    Action(""){outlineNb.enlarge.unsafePerformIO}.peer)

  getActionMap.put("ReduceAction", 
    Action(""){outlineNb.reduce.unsafePerformIO}.peer)
}

// vim: set ts=2 sw=2 et:
