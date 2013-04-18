package efa.nb.tc.install

import efa.nb.tc.EfaTc
import scalaz._, Scalaz._

class Installer extends efa.nb.module.NbModule {
  protected def closeIO = for {
    tcs ← EfaTc.registry
    _   ← tcs foldMap { _.doClose }
    _   ← efa.nb.NbSystem.shutdown
  } yield true 
}

// vim: set ts=2 sw=2 et:
