package efa.nb.controller

import dire._
import dire.swing.SwingStrategy
import dire.control.Var
import org.netbeans.spi.actions.AbstractSavable 
import scalaz._, Scalaz._, effect.{IO, IORef}

class Saver private (
  private val src: Var[Option[Unit]],
  override val findDisplayName: String,
  registered: IORef[Boolean]
) extends AbstractSavable {
  private lazy val obj = new Object{}

  private def doRegister(b: Boolean): IO[Unit] = for {
    reg ← registered.read
    _   ← registered write b
    _   ← if (b ≟ reg) IO.ioUnit
          else (b ? IO(register()) | IO(unregister()))
  } yield ()

  def sink: DataSink[Boolean] =
    DataSink.create(doRegister, IO.ioUnit, SwingStrategy)

  private def doSave: IO[Unit] =
    registered.read.ifM(src.put(().some) >> doRegister(false), IO.ioUnit)

  override def handleSave() { doSave.unsafePerformIO() }

  override def equals(other: Any) = other match {
    case s: Saver ⇒ s.obj == obj
    case _ ⇒ false
  }

  override def hashCode = obj.hashCode
}

object Saver {
  private def apply(name: String): IO[Saver] = for {
    src ← Var newVar none[Unit]
    reg ← IO newIORef false
  } yield new Saver(src, name, reg)
}

// vim: set ts=2 sw=2 et:
