package efa.nb.controller

import dire._
import dire.swing.SwingStrategy
import dire.control.Var
import dire.util.save.{SaveOut, withHandler}
import org.netbeans.spi.actions.AbstractSavable
import org.netbeans.api.actions.Savable
import scalaz._, Scalaz._, effect.IO

private[controller] final class Saver (
  val si: SavableInfo, safe: IO[Unit]
) extends AbstractSavable {
  def reg(b: Boolean) = IO(b ? register() | unregister())
  override def findDisplayName = si.name
  override def handleSave() { safe.unsafePerformIO() }
  override def hashCode = si.key.hashCode
  override def equals(o: Any) =
    o match { case s: Saver ⇒ s.si.key == si.key; case _ ⇒ false }
}

object Saver {
  def sf[A:Equal](si: SavableInfo, save: Out[A]): SF[A, SaveOut[A]] = {
      def create(s: IO[Unit], reg: Boolean) = for {
        s ← IO(new Saver(si, s))
        _ ← s reg reg
        _ ← si register (if (reg) s.right else s.left)
      } yield ()
      
      def connect(o: Out[Unit]): Out[SaveOut[A]] = _ match {
        case (s, oa@Some(a)) if s ≠ oa ⇒ create(save(a) >>= o, true)
        case _                         ⇒ create(IO.ioUnit, false)
      }

      withHandler(SF.connectOuts(connect, SwingStrategy))
    }
}

// vim: set ts=2 sw=2 et:
