package efa.nb.spi

import efa.core.Default

trait NbLoc {
  def confirmOverwrite (p: String): String

  def editTitle (s: String): String

  def fileRenamed (o: String, n: String): String

  def fileRenameError (s: String): String

  def folder: String

  def folderRenamed (o: String, n: String): String

  def folderRenameError (s: String): String

  def inputTitle: String

  def newTitle (s: String): String

} 

/**
 * Default localization (English)
 */
object NbLoc extends NbLoc {
  implicit val defInst: Default[NbLoc] = Default.default(this)

  def confirmOverwrite (p: String): String =
    s"File $p already exists. Do you want to overwrite it?"

  def editTitle (s: String) = "Edit %s" format s

  def fileRenamed (o: String, n: String) =
    "Renamed file %s to %s." format (o, n)

  def fileRenameError (s: String): String =
    "Unable to rename file to " + s

  def folder = "Folder"

  def folderRenamed (o: String, n: String) =
    "Renamed folder %s to %s." format (o, n)

  def folderRenameError (s: String): String =
    "Unable to rename folder to " + s

  def inputTitle = "Input"

  def newTitle (s: String) = "New %s" format s
}

// vim: set ts=2 sw=2 et:
