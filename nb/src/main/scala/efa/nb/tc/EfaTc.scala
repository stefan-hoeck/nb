package efa.nb.tc

import org.openide.windows.{TopComponent}
import org.openide.util.{Lookup, NbPreferences}
import java.util.prefs.Preferences
import scala.collection.mutable.{ArrayBuffer, SynchronizedBuffer}
import scalaz._, Scalaz._, effect.IO

abstract class EfaTc extends TopComponent with PersistentComponent {
  protected def initialize: IO[Unit]

  protected def cleanup: IO[Unit]

  final private[tc] def doOpen = 
    initialize >> read >> EfaTc.add(this)

  final private[tc] def doClose = 
    EfaTc.remove(this) >> persist >> cleanup

  override def componentOpened() { doOpen.unsafePerformIO() }
  
  override def componentClosed() { doClose.unsafePerformIO() }
  
  override def getPersistenceType = TopComponent.PERSISTENCE_ALWAYS
  
  protected[tc] def prefId = preferredID
}

object EfaTc {
  private[this] val reg =
    new ArrayBuffer[EfaTc] with SynchronizedBuffer[EfaTc]

  private[tc] def registry: IO[List[EfaTc]] = IO(reg.toList)

  private def add(tc: EfaTc) = IO(reg += tc)

  private def remove(tc: EfaTc) = IO(reg -= tc)
}

// vim: set ts=2 sw=2 et:
