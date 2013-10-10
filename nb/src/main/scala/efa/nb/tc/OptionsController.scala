package efa.nb.tc

import dire._
import dire.control.Var
import dire.swing._, Swing._
import dire.validation.VSIn
import efa.core.ValRes
import java.beans.{PropertyChangeSupport, PropertyChangeListener}
import javax.swing.JComponent
import org.netbeans.spi.options.OptionsPanelController
import org.openide.util.Lookup
import scalaz._, Scalaz._, effect.IO

class OptionsController[A,B](create: IO[(Elem, VSIn[A])], set: Out[A])
extends OptionsPanelController {
  private[this] val pcs = new PropertyChangeSupport(this)
  private[this] var chngd = false
  private[this] var pnl: Option[(Panel,VSIn[A])] = None
  private[this] var stop: Option[IO[Unit]] = None
  private[this] var valRes: ValRes[A] = "".failureNel
  private[this] val fire = Var newVar none[Unit] unsafePerformIO

  final override def applyChanges() { applyCs.unsafePerformIO }
  final override def cancel() { cncl.unsafePerformIO }
  final override def isValid = valRes.isSuccess
  final override def isChanged = changed.unsafePerformIO
  final override def update { doUpdate.unsafePerformIO }
  override def getHelpCtx = null

  final override def getComponent(lkp: Lookup): JComponent =
    getPnl map (_._1.peer) unsafePerformIO

  final override def addPropertyChangeListener(l: PropertyChangeListener) {
    pcs.addPropertyChangeListener(l)
  }

  final override def removePropertyChangeListener(l: PropertyChangeListener) {
    pcs.removePropertyChangeListener(l);
  }

  private[this] def getPnl: IO[(Panel,VSIn[A])] = for {
    op   ← IO(pnl)
    res  ← op.fold(createPnl)(IO(_))
  } yield res

  private[this] def clear: IO[Unit] = for {
    _ ← stop.orZero
    _ ← IO(stop = None)
  } yield ()

  private[this] def changed = IO(chngd)

  private[this] def handleValRes(v: ValRes[A]) =
    IO(valRes = v) >> setChanged(true)

  private[this] def setChanged(b: Boolean) = IO {
    if (chngd != b) {
      chngd = b
      pcs.firePropertyChange(OptionsPanelController.PROP_CHANGED, !b, b)
    }

    pcs.firePropertyChange(OptionsPanelController.PROP_VALID, null, null)
  }
  
  private[this] def doUpdate = for {
    _  ← setChanged(false)
    _  ← clear
    p  ← getPnl
    k  ← efa.nb.NbSystem forever p._2
    _  ← IO(stop = k.some)
  } yield ()

  private[this] def cncl: IO[Unit] = clear >> setChanged(false)

  private[this] def applyCs: IO[Unit] = for {
    _   ← fire put ().some
    _   ← clear
    _   ← setChanged(false)
  } yield ()

  private[this] def createPnl: IO[(Panel,VSIn[A])] = for {
    p     ← create
    panel ← p._1.panel
    _     ← IO(pnl = (panel, p._2).some)
  } yield (panel, p._2)

  private[this] def sf(in: VSIn[A]) = {
    val fireIn = fire.in collectO identity

    (in syncTo handleValRes).collectS on fireIn syncTo set
  }
}

// vim: set ts=2 sw=2 et:
