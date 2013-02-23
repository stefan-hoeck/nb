package efa.nb.local.de

import efa.nb.spi.NbLoc

class NbLocal extends NbLoc {

  def editTitle (s: String) = "%s editieren" format s

  def fileRenamed (o: String, n: String) =
    "Datei %s umbenennt nach %s." format (o, n)

  def fileRenameError (s: String): String =
    "Datei konnte nicht nach %s umbenannt werden" format s

  def folder = "Ordner"

  def folderRenamed (o: String, n: String) =
    "Ordner %s umbenennt nach %s." format (o, n)

  def folderRenameError (s: String): String =
    "Ordner konnte nicht nach %s umbenannt werden" format s

  def inputTitle = "Eingabe"

  def newTitle (s: String) = "Neu: %s" format s
}

// vim: set ts=2 sw=2 et:
