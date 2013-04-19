package efa.nb.node

import efa.core.Efa._
import org.openide.filesystems.FileObject
import scalaz._, Scalaz._, effect.IO

case class FolderInfo (
  filter: FileObject ⇒ Boolean,
  destroyer: FileObject ⇒ Destroyer,
  renamer: FileObject ⇒ Renamer,
  newTypes: FileObject ⇒ List[NtInfo],
  pasters: FileObject ⇒ List[Paster],
  contextRoots: List[String],
  folderSorter: List[FileObject] ⇒ List[FileObject],
  fileSorter: List[FileObject] ⇒ List[FileObject]
) {
  def adjustNode (f: FolderNode, fo: FileObject): IO[Unit] = for {
    _ ← f setDestroyer destroyer(fo)
    _ ← f setRenamer renamer(fo)
    _ ← f setNewTypes newTypes(fo)
    _ ← f setPasters pasters(fo)
    _ ← f contextRoots contextRoots
    _ ← IO(f.setDisplayName(fo.getName))
  } yield ()
}


object FolderInfo {
  val immutableName: FolderInfo = sortedByName (
    _ ⇒ true, _ ⇒ None, _ ⇒ None, _ ⇒ Nil, _ ⇒ Nil, Nil
  )

  val immutablePosition: FolderInfo = sortedByPosition (
    _ ⇒ true, _ ⇒ None, _ ⇒ None, _ ⇒ Nil, _ ⇒ Nil, Nil
  )

  def sortedByName (
    filter: FileObject ⇒ Boolean,
    destroyer: FileObject ⇒ Destroyer,
    renamer: FileObject ⇒ Renamer,
    newTypes: FileObject ⇒ List[NtInfo],
    pasters: FileObject ⇒ List[Paster],
    contextRoots: List[String]
  ): FolderInfo = FolderInfo (
    filter, destroyer, renamer, newTypes, pasters, contextRoots,
    _ sortBy (_.getName), _ sortBy (_.getNameExt)
  )

  def sortedByPosition (
    filter: FileObject ⇒ Boolean,
    destroyer: FileObject ⇒ Destroyer,
    renamer: FileObject ⇒ Renamer,
    newTypes: FileObject ⇒ List[NtInfo],
    pasters: FileObject ⇒ List[Paster],
    contextRoots: List[String]
  ): FolderInfo = FolderInfo (
    filter, destroyer, renamer, newTypes, pasters, contextRoots,
    _ sortBy pos, _ sortBy pos 
  )

  private def pos(o: FileObject): Int =
    o.getAttribute("position").toString.read[Int] | Int.MinValue
}

// vim: set ts=2 sw=2 et:
