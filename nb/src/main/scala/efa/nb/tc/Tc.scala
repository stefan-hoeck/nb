package efa.nb.tc

import org.openide.explorer.ExplorerManager
import org.openide.windows.TopComponent
import java.awt.BorderLayout
import scala.collection.mutable.{ArrayBuffer, SynchronizedBuffer}
import scala.collection.JavaConverters._
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
    A.read(a) >> A.initialize(a)(cleanup write _) >> Tc.add(this)

  final private[tc] def doClose = 
    Tc.remove(this) >>
    A.persist(a) >>
    cleanup.read.μ >>
    cleanup.write(IO.ioUnit)

  final private[tc] def doPersist = A.persist(a)

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
  private[this] lazy val reg =
    new java.util.concurrent.ConcurrentLinkedQueue[Tc[_]]

  private[tc] def registry: IO[List[Tc[_]]] = IO(reg.asScala.toList)

  private def add(tc: Tc[_]) = IO(reg.add(tc))

  private def remove(tc: Tc[_]) = IO(reg.remove(tc))
}

// vim: set ts=2 sw=2 et:
