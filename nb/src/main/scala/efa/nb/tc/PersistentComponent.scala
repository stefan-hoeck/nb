package efa.nb.tc

//import efa.core.Log, efa.core.Efa._
//import efa.io._
//import java.util.prefs.Preferences
//import org.openide.util.NbPreferences
//import scalaz._, Scalaz._, effect._
//
//trait PersistentComponent extends LogDisIOFunctions with LogDisIOInstances {
//
//  final def read: IO[Unit] = {
//    def doRead = for {
//      _ ← info ("Reading " + prefId)
//      p ← prefs
//      _ ← (p.get(prefId + "%s version", null) == version) ?
//          readProps(p) |
//          ldiUnit
//      _ ← liftIO(persistentChildren foldMap (_.read))
//    } yield ()
//
//    logger >>= (_ logDisZ doRead)
//  }
//
//  protected def logger: IO[LoggerIO] = efa.nb.pref.tcLogger
//
//  protected def persistentChildren: List[PersistentComponent] = Nil 
//
//  protected def prefId: String
//
//  protected def version: String
//  
//  protected def prefs: LogDisIO[Preferences] = {
//    val res = liftIO (IO (NbPreferences.forModule(getClass)))
//    except (res, _ ⇒ "Unable to load preferences for " + getClass)
//  }
//  
//  protected def readProps(prefs: Preferences): LogDisIO[Unit] = ldiUnit
//  
//  protected def writeProps(prefs: Preferences): LogDisIO[Unit] = ldiUnit
//  
//  final def persist: IO[Unit] = {
//    def doPersist = for {
//      _ ← info ("Persisting " + prefId)
//      p ← prefs
//      _ ← liftIO (IO (p.put(prefId + "%s version", version)))
//      _ ← writeProps(p)
//      _ ← liftIO (persistentChildren foldMap (_.persist))
//    } yield ()
//
//    logger >>= (_ logDisZ doPersist)
//  }
//}

// vim: set ts=2 sw=2 et:
