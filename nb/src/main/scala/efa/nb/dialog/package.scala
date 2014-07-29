package efa.nb

import dire.swing.Elem
import dire.validation.VSIn
import efa.io._
import java.io.File
import scalaz._, Scalaz._, effect.IO

package object dialog {
  type DEInfo[A] = scalaz.effect.IO[(Elem, VSIn[A])]

  def saveFile(
    desc: String,
    selected: Option[String],
    exts: String*
  ): LogDisIO[Option[File]] =
    IOChooser.filter(desc, selected, exts: _*) saveConfirm confirm

  def saveTxt(selected: Option[String]): LogDisIO[Option[File]] = 
    IOChooser.txtOnly(selected) saveConfirm confirm

  def saveAny(selected: Option[String]): LogDisIO[Option[File]] = 
    IOChooser.all(selected) saveConfirm confirm

  private def confirm(p: String): IO[Boolean] =
    Confirmation msg efa.nb.loc.confirmOverwrite(p)
}

// vim: set ts=2 sw=2 et:
