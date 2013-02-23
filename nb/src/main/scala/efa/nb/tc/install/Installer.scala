package efa.nb.tc.install

import efa.nb.tc.EfaTc
import org.openide.modules.ModuleInstall
import scalaz._, Scalaz._, effect._

class Installer extends ModuleInstall {

  override def closing() = {
    val res: IO[Unit] = EfaTc.registry foldMap (_.persist)
    res.unsafePerformIO
    true
  }
}

// vim: set ts=2 sw=2 et:
