package efa.nb.tc

import dire.swing.{Component, AsElem, AsSingleElem, Elem}
import javax.swing.JToolBar
import org.openide.util.Utilities
import scala.collection.JavaConverters._
import scalaz._, Scalaz._
import scalaz.effect.IO

case class ToolBar(peer: JToolBar)

object ToolBar {
  def apply(paths: List[String]): IO[ToolBar] = IO {
    val peer = new JToolBar()
    val as = paths flatMap forPath
    
    as foreach { a ⇒ peer.add(a) }

    ToolBar(peer)
  }

  private[this] def forPath(p: String) =
    Utilities.actionsForPath(p).asScala.toList

  implicit val ToolBarComponent: Component[ToolBar] =
    new Component[ToolBar] {
      def peer(tb: ToolBar) = tb.peer
    }

  implicit val ToolBarAsElem: AsSingleElem[ToolBar] =
    Elem.asSingle(tb ⇒ Elem.Single(tb.peer))
}
// vim: set ts=2 sw=2 et:
