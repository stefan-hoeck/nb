package efa.nb.tc

import org.openide.windows.{TopComponent, WindowManager}
import org.openide.util.{Lookup, NbPreferences}
import java.util.prefs.Preferences

abstract class EfaTc extends TopComponent with PersistentComponent {
  
  import EfaTc.reg

  reg = this :: reg
  
  override def componentOpened() { read.unsafePerformIO }
  
  override def componentClosed() { persist.unsafePerformIO }
  
  override def getPersistenceType = TopComponent.PERSISTENCE_ALWAYS
  
  protected[tc] def prefId = preferredID

}

object EfaTc {
  private var reg: List[EfaTc] = Nil
  private[tc] def registry = reg
}

// vim: set ts=2 sw=2 et:
