package efa.nb.node

import efa.react.Out
import java.awt.Image
import java.awt.datatransfer.Transferable
import java.awt.dnd.DnDConstants
import java.util.{List ⇒ JList}
import javax.swing.Action
import org.openide.nodes.{Children, AbstractNode, Sheet, Node, NodeTransfer}
import org.openide.util.Lookup
import org.openide.util.datatransfer.{NewType, PasteType ⇒ JPasteType}
import scalaz._, Scalaz._, effect.IO

abstract class PureNode(c: Children, l: Lookup)
   extends AbstractNode(c, l) {

  // *** Destroy ***
  private[this] var destroyer: Destroyer = None

  final def setDestroyer: Out[Destroyer] = d ⇒ IO{destroyer = d}

  final def onDestroy: Out[IO[Unit]] = setDestroyer ∙ (_.some)
  
  override final def canDestroy = destroyer.nonEmpty

  override final def destroy() { destroyer foreach (_.unsafePerformIO) }

  // *** Rename ***
  private[this] var renamer: Renamer = None

  final def setRenamer: Out[Renamer] = r ⇒  IO{ renamer = r }

  final def onRename: Out[String ⇒ IO[Unit]] = setRenamer ∙ (_.some)

  override final def canRename = renamer.nonEmpty

  override final def setName (s: String) { renamer foreach (_(s).unsafePerformIO) }
  
  // *** NewTypes ***
  type Nt = org.openide.util.datatransfer.NewType

  private[this] var ntInfos: List[NtInfo] = Nil

  final def addNewType: Out[NtInfo] = ni ⇒  IO{ ntInfos ::= ni }

  final def setNewTypes: Out[List[NtInfo]] = ns ⇒  IO{ ntInfos = ns }

  override final def getNewTypes = {
    def toNt = (i: NtInfo) ⇒ new Nt {
      override def getName = i._1
      override def create() { i._2.unsafePerformIO }
    }

    ntInfos map toNt toArray
  }
 
  // *** Cut, Copy, Paste ***

  private[this] var pasters: List[Paster] = Nil

  final def setPasters: Out[List[Paster]] = ps ⇒ IO{ pasters = ps }

  override final def getDropType (t: Transferable, a: Int, i: Int) = new JPasteType {
    override def paste(): Transferable = {
      def infoO = PasteType.values foldMap (_ info t)
      def io = infoO map (i ⇒ pasters foldMap (_ tupled i)) orZero

      io.unsafePerformIO
      null
    }
  }

  override final protected def createPasteTypes(
    t: Transferable, s: JList[JPasteType]
  ) {
    super.createPasteTypes(t, s)
    Option(getDropType( t, DnDConstants.ACTION_COPY, -1 )) foreach {s add _}
  }

  private[this] var prefAction: Option[Action] = None

  final def setPreferredAction (oa: Option[Action]): IO[Unit] =
    IO(prefAction = oa)

  final override def getPreferredAction =
    prefAction getOrElse super.getPreferredAction

  private[this] var iconImage: Option[IconImageF] = None

  final def setIconImage (o: Option[IconImageF]): IO[Unit] =
    IO(iconImage = o)

  final override def getIcon(t: Int) = 
    iconImage flatMap (_ apply t unsafePerformIO) getOrElse super.getIcon(t)
}

// vim: set ts=2 sw=2 et:
