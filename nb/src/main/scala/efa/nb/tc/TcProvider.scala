package efa.nb.tc

import efa.nb.pref
import org.openide.windows.{TopComponent, WindowManager}
import scalaz._, Scalaz._, effect._

abstract class TcProvider[A:AsTc,B](implicit m: Manifest[B]) {
  import TcProvider._

  private[this] lazy val inst = tcc.newInstance.asInstanceOf[B]

  private[this] val tcc = m.runtimeClass

  private[this] def getDef: IO[B] = for {
    l    ← pref.tcLogger
    _    ← l debug s"Get default incstance for $id"
  } yield inst

  private[this] def findInst: IO[B] = for {
    l  ← pref.tcLogger
    _  ← l debug (s"Searching instance for $id")
    tc ← IO(WindowManager.getDefault().findTopComponent(id))
    r  ← tc match {
           case null ⇒  l.warn(notFound(id)) >> getDef
           case w if(tcc isAssignableFrom w.getClass) ⇒ IO(w.asInstanceOf[B])
           case _ ⇒ l.warn(multipleFound(id)) >> getDef
         }
  } yield r

  def id = AsTc[A].preferredId

  /**
   * Gets default instance. Do not use directly:
   * reserved for *.settings files only,
   * i.e. deserialization routines; otherwise you could
   * get a non-deserialized instance.
   * To obtain the singleton instance, use {@link #findInstance}.
   */
  def getDefault: B = getDef.unsafePerformIO

  /**
   * Obtain the TopComponent instance.
   * Never call {@link #getDefault} directly!
   */
  def findInstance: B = findInst.unsafePerformIO
}

object TcProvider {
  private def notFound (id: String) = 
    "Cannot find " + id + 
    " component. It will not be located properly in the window system."

  private def multipleFound (id: String) = 
    "There seem to be multiple components with the '" + id +
    "' ID. That is a potential source of errors and unexpected behavior."
}

// vim: set ts=2 sw=2 et:
