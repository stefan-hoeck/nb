package efa.nb.tc

import efa.core.Log, efa.core.Efa._
import efa.io._, EfaIO._
import java.util.prefs.Preferences
import org.openide.util.NbPreferences
import scalaz._, Scalaz._, effect._

trait Persistent[A] {
  def version: String

  def preferredId: String

  lazy val tag = s"$preferredId %s $version"

  final def read(a: A): IO[Unit] = {
    def doRead = for {
      _ ← info(s"Reading $preferredId")
      p ← prefs
      _ ← (p.get(tag, null) == version) ? readProps(a).run(p) | ldiUnit
    } yield ()

    logger >>= (_ logDisZ doRead)
  }

  protected def logger: IO[LoggerIO] = efa.nb.pref.tcLogger
  
  protected def prefs: LogDisIO[Preferences] = {
    val res = point(NbPreferences forModule getClass)
    except(res, _ ⇒ "Unable to load preferences for " + getClass)
  }
  
  protected def readProps(a: A): WithPrefs[Unit] = withPrefs(_ ⇒ ldiUnit)
  
  protected def writeProps(a: A): WithPrefs[Unit] = withPrefs(_ ⇒ ldiUnit)
  
  final def persist(a: A): IO[Unit] = {
    def doPersist = for {
      _ ← info (s"Persisting $preferredId")
      p ← prefs
      _ ← point(p.put(tag, version))
      _ ← writeProps(a) run p
    } yield ()

    logger >>= (_ logDisZ doPersist)
  }
}

object Persistent {
  def apply[A](implicit A: Persistent[A]): Persistent[A] = A
}

// vim: set ts=2 sw=2 et:
