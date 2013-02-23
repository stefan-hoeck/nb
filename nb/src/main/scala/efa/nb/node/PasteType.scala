package efa.nb.node

import java.awt.datatransfer.Transferable
import java.awt.dnd.DnDConstants
import org.openide.nodes.{NodeTransfer, Node}
import scalaz._, Scalaz._

sealed abstract class PasteType (val const: Int) {
  def info (t: Transferable): FirstOption[(PasteType, Node)] =
    Option(NodeTransfer.node(t, const)) map ((this, _)) first
}

object PasteType {
  case object Cut extends PasteType(NodeTransfer.CLIPBOARD_CUT)
  case object Copy extends PasteType(DnDConstants.ACTION_COPY)
  case object Move extends PasteType(DnDConstants.ACTION_MOVE)

  val values = List[PasteType](Cut, Move, Copy)
}

// vim: set ts=2 sw=2 et:
