package efa.nb.controller

import dire.Out
import org.netbeans.api.actions.Savable
import scalaz.\/
import scalaz.effect.IO

final case class SavableInfo(
    key: AnyRef,
    name: String,
    register: Out[Savable \/ Savable])

// vim: set ts=2 sw=2 et:
