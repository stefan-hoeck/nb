package efa.nb.controller

import org.netbeans.spi.actions.AbstractSavable 
import efa.react.{Source, EET, RTrans, IOCFunctions}
import scalaz.syntax.monad._
import scalaz.effect.{IO, IORef}

class Saver private (
  private val src: Source[SaveEvent],
  override val findDisplayName: String,
  perform: IORef[IO[Unit]],
  registered: IORef[Boolean]
) extends AbstractSavable {
  private lazy val obj = new Object{}

  private def doRegister: IO[Unit] = 
    IO(register()) >>
    registered.write(true) >>
    (src fire Registered(this))

  private def doUnregister: IO[Unit] =
    IO(unregister()) >>
    registered.write(false) >>
    (src fire Unregistered(this))

  def fire (io: IO[Unit]): IO[Unit] = 
    perform.write(io) >> registered.read.ifM (IO.ioUnit, doRegister)

  override def handleSave() {
    perform.read.μ >>
    registered.read.ifM(doUnregister, IO.ioUnit) unsafePerformIO
  }

  override def equals(other: Any) = other match {
    case s: Saver ⇒ s.obj == obj
    case _ ⇒ false
  }

  override def hashCode = obj.hashCode
}

object Saver extends IOCFunctions {
  def events (name: String): EET[IO[Unit],SaveEvent] = RTrans(es ⇒
    for {
      s ← liftIO (apply(name))
      _ ← es --> s.fire
    } yield s.src
  )

  private def apply (name: String): IO[Saver] = for {
    src ← Source[SaveEvent]
    per ← IO newIORef IO.ioUnit
    reg ← IO newIORef false
  } yield new Saver(src, name, per, reg)
}

// vim: set ts=2 sw=2 et:
