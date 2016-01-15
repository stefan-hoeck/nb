package efa.nb.tc.install

import efa.nb.tc.Tc
import scalaz._, Scalaz._

class Installer extends efa.nb.module.NbModule {
  override protected def closeIO = for {
    tcs ← Tc.registry
    _   ← tcs foldMap { _.doClose }
    _   ← efa.nb.NbSystem.shutdown
  } yield ()
}

// vim: set ts=2 sw=2 et:
