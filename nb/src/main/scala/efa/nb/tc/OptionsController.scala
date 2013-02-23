package efa.nb.tc

import efa.core.ValRes
import efa.react.{SIn, Out, Connectors, Signal, sTrans}
import java.beans.{PropertyChangeSupport, PropertyChangeListener}
import javax.swing.JComponent
import org.netbeans.spi.options.OptionsPanelController
import org.openide.util.Lookup
import scalaz._, Scalaz._, effect.IO

class OptionsController[A,B] (
  create: IO[B],
  comp: B ⇒ JComponent,
  in: B ⇒ SIn[ValRes[A]],
  set: Out[A]
)extends OptionsPanelController {
  private[this] val pcs = new PropertyChangeSupport(this)
  private[this] var chngd = false
  private[this] var pnl: Option[B] = None
  private[this] var cs: Option[(Connectors,Signal[ValRes[A]])] = None

  final override def applyChanges() { applyCs.unsafePerformIO }
  final override def cancel() { cncl.unsafePerformIO }
  final override def isValid = valid.unsafePerformIO
  final override def isChanged = changed.unsafePerformIO
  final override def update { doUpdate.unsafePerformIO }
  override def getHelpCtx = null

  final override def getComponent(lkp: Lookup): JComponent =
    getPnl map comp unsafePerformIO

  final override def addPropertyChangeListener(l: PropertyChangeListener) {
    pcs.addPropertyChangeListener(l)
  }

  final override def removePropertyChangeListener(l: PropertyChangeListener) {
    pcs.removePropertyChangeListener(l);
  }

  private[this] def getPnl: IO[B] = for {
    op   ← IO(pnl)
    res  ← op.fold(createPnl)(IO(_))
  } yield res

  private[this] def clear: IO[Unit] = for {
    ocs ← IO(cs)
    _   ← IO(cs = None)
    _   ← ocs ∘ (_._1.toList.foldMap (_.disconnect)) orZero
  } yield ()

  private[this] def changed = IO(chngd)

  private[this] def setChanged (b: Boolean) = IO {
    if (chngd != b) {
      chngd = b
      pcs.firePropertyChange(OptionsPanelController.PROP_CHANGED, !b, b)
    }

    pcs.firePropertyChange(OptionsPanelController.PROP_VALID, null, null)
  }
  
  private[this] def doUpdate = for {
    _  ← setChanged(false)
    _  ← clear
    b  ← getPnl
    p  ← in(b) spinoff changedSet runIO () 
    _  ← IO(cs = p.some)
  } yield ()

  private[this] def cncl: IO[Unit] = clear >> setChanged(false)

  private[this] def applyCs: IO[Unit] = for {
    ocs ← IO(cs)
    _   ← clear
    _   ← setChanged(false)
    _   ← ocs ∘ (_._2.now >>= setV) orZero
  } yield ()

  private[this] def setV (v: ValRes[A]): IO[Unit] =
    v.fold (_ ⇒ IO.ioUnit, set)

  private[this] def valid: IO[Boolean] = for {
    ocs ← IO(cs)
    b   ← ocs.fold(IO(false))(_._2.now ∘ (_.isSuccess))
  } yield b
    
  private[this] def createPnl: IO[B] = for {
    p  ← create
    _  ← IO(pnl = p.some)
  } yield p

  private[this] def changedSet =
    sTrans.id[ValRes[A]].events as true to setChanged
}

// vim: set ts=2 sw=2 et:
