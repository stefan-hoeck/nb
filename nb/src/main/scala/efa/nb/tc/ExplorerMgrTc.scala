package efa.nb.tc

import org.openide.explorer.{ExplorerManager, ExplorerUtils}
import org.openide.nodes.Node
import org.openide.util.Lookup
import org.openide.util.lookup.ProxyLookup
import scalaz._, Scalaz._, effect.IO

abstract class ExplorerMgrTc(val rootNode: Node, lkp: Option[Lookup])
   extends EfaTc with ExplorerManager.Provider {
  
  override lazy val getExplorerManager = {
    val res = new ExplorerManager
    def explorerLookup: Lookup =
      ExplorerUtils createLookup (res, getActionMap)

    def myLookup: Lookup =
      lkp.fold(explorerLookup)(new ProxyLookup (_, explorerLookup))

    associateLookup(myLookup)
    res setRootContext rootNode

    res
  }
}

// vim: set ts=2 sw=2 et:
