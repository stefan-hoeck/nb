package efa.nb.tc

import dire.swing.Component
import org.openide.awt.UndoRedo
import org.openide.explorer.ExplorerManager
import org.openide.nodes.Node
import org.openide.util.Lookup
import scalaz.effect.IO

/** Type class to represent a thing as a TopComponent */
trait AsTc[A] extends Component[A] with Persistent[A] {

  def create: IO[A]

  def explorerMgr(a: A): Option[ExplorerManager] = None

  /** Initializes component before being opened.
    *
    * This happens before persisted data of the component is read.
    */
  def initialize(a: A): (IO[Unit] ⇒ IO[Unit]) ⇒ IO[Unit]

  /** Returns the `Lookup` associated with the component */
  def lookup(a: A): Lookup = Lookup.EMPTY

  /** Returns the localized name of a component */
  def name: String

  /** Returns the persistence type of the component */
  def persistenceType: PersistenceType = PersistenceType.Always

  /** Returns a tooltip text associated with the component */
  def tooltip: String = ""

  /** Returns the `UndoRedo` (if any) associated with the component */
  def undoRedo(a: A): Option[UndoRedo] = None
}

object AsTc {
  def apply[A](implicit A: AsTc[A]): AsTc[A] = A
}

// vim: set ts=2 sw=2 et:
