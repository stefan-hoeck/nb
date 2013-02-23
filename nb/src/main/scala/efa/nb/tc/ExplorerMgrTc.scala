package efa.nb.tc

import org.openide.explorer.{ExplorerManager, ExplorerUtils}
import org.openide.nodes.Node
import org.openide.util.Lookup
import org.openide.util.lookup.ProxyLookup
import scalaz._, Scalaz._

abstract class ExplorerMgrTc(rn: ⇒ Node, lkp: Option[Lookup])
   extends EfaTc with ExplorerManager.Provider {
  protected def rootNode: Node = rn
  
  override lazy val getExplorerManager = {
    val res = new ExplorerManager
    def explorerLookup: Lookup =
      ExplorerUtils createLookup (res, getActionMap)

    def myLookup: Lookup =
      lkp.fold(explorerLookup)(new ProxyLookup (_, explorerLookup))

    associateLookup(myLookup)
    res setRootContext rn

    res
  }
}

// vim: set ts=2 sw=2 et:
