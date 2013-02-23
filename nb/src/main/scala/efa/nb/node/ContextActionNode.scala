package efa.nb.node

import efa.core.Efa._
import efa.react.Out
import javax.swing.{Action, JSeparator}
import org.openide.util.lookup.Lookups
import scalaz._, Scalaz._, effect.IO

trait ContextActionNode extends org.openide.nodes.Node {

  // *** Actions ***

  private[this] var crs: List[String] = List.empty

  private[node] def getCrs: IO[List[String]] = IO(crs)

  final def contextRoots: Out[List[String]] = ss ⇒ IO(crs = ss)

  override final def getActions(context: Boolean): Array[Action] = {
    def toActions(path: String): IO[Seq[Action]] = 
      (Lookups forPath path).all[Object] map (_ flatMap singleToAction)

    def singleToAction(o: AnyRef): Seq[Action] = o match {
      case j: JSeparator ⇒ Seq(null)
      case ac: Action    ⇒ Seq(ac)
      case _             ⇒ Seq.empty
    }

    crs traverse toActions map (_.flatten.toArray) unsafePerformIO
  }
}

// vim: set ts=2 sw=2 et:
