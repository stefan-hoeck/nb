package efa.nb.tc.install

class Installer extends efa.nb.module.NbModule {
  protected def closeIO = efa.nb.NbSystem.shutdown map { _ ⇒ true }
}

// vim: set ts=2 sw=2 et:
