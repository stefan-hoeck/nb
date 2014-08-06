package efa.nb.actions

import efa.core.Unerased
import efa.core.syntax.lookup
import org.openide.util.HelpCtx
import org.openide.util.actions.CookieAction
import org.openide.nodes.Node
import scalaz._, Scalaz._, effect._

abstract class CookieAllAction[A:Unerased](
  val getName: String,
  caMode: CookieActionMode = CookieActionMode.All
) extends CookieAction {
  override final protected def mode = caMode.value

  override final protected def cookieClasses = Array(Unerased[A].clazz)

  override final protected def performAction(nodes: Array[Node]) {
    nodes.toList foldMap (_.getLookup.all[A]) flatMap run unsafePerformIO
  }

  def run (as: Seq[A]): IO[Unit]

  override def getHelpCtx = HelpCtx.DEFAULT_HELP
  override protected def asynchronous = false
  override protected def surviveFocusChange = false
}

// vim: set ts=2 sw=2 et:
