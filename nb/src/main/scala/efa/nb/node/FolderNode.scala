package efa.nb.node

import scala.collection.JavaConversions._
import org.openide.util.Lookup
import org.openide.loaders.{DataObject, DataObjectNotFoundException}
import org.openide.filesystems.{FileChangeListener, FileLock, FileObject, FileEvent, 
                                FileRenameEvent, FileAttributeEvent, FileUtil}
import org.openide.nodes.{Children, Node}
import scalaz._, Scalaz._, effect.IO

final class FolderNode private (c: FolderChildren)
   extends PureNode(c, Lookup.EMPTY) with ContextActionNode

object FolderNode {
  def root (
    fo: FileObject,
    fiRoot: FolderInfo,
    fiChild: FolderInfo
  ): IO[Node] = {
    require(fo.isFolder)

    for {
      c ← IO(new FolderChildren(fo, fiChild))
      n ← IO(new FolderNode(c))
      _ ← fiRoot adjustNode (n, fo)
    } yield n
  }

  private[node] def child (fi: FolderInfo, fo: FileObject): IO[Array[Node]] =
    root(fo, fi, fi) map (Array(_))

  def forLayerPath (p: String): IO[Node] = root (
    FileUtil getConfigFile p,
    FolderInfo.immutablePosition,
    FolderInfo.immutablePosition
  )
}

private[node] final class FolderChildren(fo: FileObject, fi: FolderInfo) 
   extends Children.Keys[FileObject] with FileChangeListener {
  assert(fo.isFolder)
  fo addFileChangeListener this
  
  private var block = false

  override protected def addNotify (): Unit = {
    val (folders, files) = fo.getChildren.toList partition (_.isFolder)
    def sortedFolders = fi folderSorter folders
    def sortedFiles = fi fileSorter (files filter fi.filter)

    setKeys(sortedFolders ::: sortedFiles toArray)
  }

  override protected def createNodes(key: FileObject): Array[Node] =
    create(key).unsafePerformIO()

  private def create (key: FileObject): IO[Array[Node]] =
    if (key.isFolder) FolderNode child (fi, key)
    else nodeForFile(key)

  private def nodeForFile (f: FileObject): IO[Array[Node]] = try {
    DataObject find f match {
      case null ⇒ IO(Array.empty)
      case d    ⇒ IO(Array(d.getNodeDelegate))
    }
  } catch { case e: DataObjectNotFoundException ⇒ IO(Array.empty)}

  override def fileFolderCreated(fe: FileEvent) = if(!block) addNotify()
  override def fileDataCreated(fe: FileEvent) = if(!block) addNotify()
  override def fileChanged(fe: FileEvent) = if(!block) addNotify()
  override def fileDeleted(fe: FileEvent) = if(!block) addNotify()
  override def fileRenamed(fe: FileRenameEvent) = if(!block) addNotify()
  override def fileAttributeChanged(fe: FileAttributeEvent) = if(!block) addNotify()
}

// vim: set ts=2 sw=2 et:
