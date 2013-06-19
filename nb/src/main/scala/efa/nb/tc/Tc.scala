package efa.nb.tc

import org.openide.explorer.{ExplorerManager, ExplorerUtils}
import org.openide.util.{Lookup, NbPreferences}
import org.openide.util.lookup.ProxyLookup
import org.openide.windows.TopComponent
import java.awt.BorderLayout
import java.util.prefs.Preferences
import scala.collection.mutable.{ArrayBuffer, SynchronizedBuffer}
import scalaz._, Scalaz._, effect.IO

abstract class Tc[A](implicit A: AsTc[A])
  extends TopComponent 
  with ExplorerManager.Provider {

  private[this] var uninitialized = true

  private[this] final val cleanup = 
    IO newIORef IO.ioUnit unsafePerformIO()

  final lazy val a: A = A.create.unsafePerformIO()

  final override lazy val getExplorerManager = 
    A explorerMgr a getOrElse new ExplorerManager

  final private[tc] def doOpen = 
    A.initialize(a)(cleanup write _) >> A.read(a) >> Tc.add(this)

  final private[tc] def doClose = 
    Tc.remove(this) >>
    A.persist(a) >>
    cleanup.read.μ >>
    cleanup.write(IO.ioUnit)

  final override def componentOpened() { 
    if (uninitialized) {
      uninitialized = false
      associateLookup(A lookup a)
      setName(A.name)
      setToolTipText(A.tooltip)
      setLayout(new BorderLayout)
      add(A peer a, BorderLayout.CENTER)
    }

    doOpen.unsafePerformIO()
  }
  
  final override def componentClosed() { doClose.unsafePerformIO() }
  
  final override def getPersistenceType = A.persistenceType.v
  
  final override def preferredID = A.preferredId

  final override def getUndoRedo = A undoRedo a getOrElse super.getUndoRedo

}

object Tc {
  private[this] val reg =
    new ArrayBuffer[Tc[_]] with SynchronizedBuffer[Tc[_]]

  private[tc] def registry: IO[List[Tc[_]]] = IO(reg.toList)

  private def add(tc: Tc[_]) = IO(reg += tc)

  private def remove(tc: Tc[_]) = IO(reg -= tc)
}

// vim: set ts=2 sw=2 et:
