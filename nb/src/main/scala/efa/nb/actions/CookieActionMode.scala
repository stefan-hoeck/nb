package efa.nb.actions

import org.openide.util.actions.CookieAction

sealed abstract class CookieActionMode (val value: Int)

object CookieActionMode {
  case object One extends CookieActionMode(CookieAction.MODE_ONE)
  case object Some extends CookieActionMode(CookieAction.MODE_SOME)
  case object All extends CookieActionMode(CookieAction.MODE_ALL)
  case object ExactlyOne extends CookieActionMode(CookieAction.MODE_EXACTLY_ONE)
  case object Any extends CookieActionMode(CookieAction.MODE_ANY)
}

// vim: set ts=2 sw=2 et:
