package efa.nb.tc

import org.openide.windows.TopComponent._

sealed abstract class PersistenceType(val v: Int)

object PersistenceType {
  case object Always extends PersistenceType(PERSISTENCE_ALWAYS)
  case object OnlyOpened extends PersistenceType(PERSISTENCE_ONLY_OPENED)
  case object Never extends PersistenceType(PERSISTENCE_NEVER)
}

// vim: set ts=2 sw=2 et:
