package efa.nb.tc

import java.util.Properties
import java.util.prefs.Preferences
import org.openide.explorer.view.OutlineView
import scala.collection.JavaConversions._
import scalaz._, Scalaz._, effect._

trait PersistentOutline extends PersistentComponent {
  import PersistentOutline._, OutlineNb._
  protected def outlineNb: OutlineNb

  private def outline = outlineNb.peer
  
  private[this] lazy val prefix = prefId + OutlineNb.OutlineView
  
  override protected def writeProps(prefs: Preferences) = for {
    rh ← liftIO(outlineNb.rowHeight)
    _  ← point(prefs.putInt(prefId + RowHeight, rh))
    ps ← point(new Properties)
    _  ← point(outline.writeSettings(ps, prefix))
    _  ← point(prefs.putByteArray(prefix, propsToArray(ps)))
  } yield ()

  override protected def readProps(prefs: Preferences) = for {
    rh ← point (prefs.getInt(prefId + RowHeight, MinRowHeight))
    _  ← liftIO(outlineNb rowHeightSet rh)
    _  ← try {
           val ps = propsFromArray(prefs.getByteArray(prefix, Array()))
           outline.readSettings(ps, prefix)
           ldiUnit 
         } catch {
           //do nothing, happens when no settings where stored
           case e: NullPointerException ⇒ ldiUnit
           case e: Exception ⇒  warning(readError(e))
         }
  } yield ()

  private def readError(e: Exception) = 
    "Error when reading props for %s: %s" format (prefId, e.toString)
}

object PersistentOutline {
  import java.io.{ByteArrayOutputStream, ByteArrayInputStream}

  private def propsToArray(ps: Properties): Array[Byte] = {
    val os = new ByteArrayOutputStream

    ps.store(os, "")
    os.close()

    os.toByteArray
  }

  private def propsFromArray (bs: Array[Byte]): Properties = {
    val in = new ByteArrayInputStream(bs)
    val ps = new Properties

    ps.load(in)
    in.close()

    ps
  }
}

// vim: set ts=2 sw=2 et:
