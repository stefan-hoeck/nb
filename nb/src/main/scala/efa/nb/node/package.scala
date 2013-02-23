package efa.nb

import efa.core._
import efa.react._
import java.awt.Image
import scalaz._, Scalaz._, effect._

package object node {
  type DataCookie = PureLookup ⇒ Option[IO[Unit]]

  type Destroyer = Option[IO[Unit]]

  type Editor = Option[IO[Unit]]

  type NtInfo = Pair[String,IO[Unit]]

  type Paster = (PasteType, org.openide.nodes.Node) ⇒ IO[Unit]

  type Renamer = Option[String ⇒ IO[Unit]]

  type ValOut[A,B] = NodeOut[A,ValRes[B]]

  type ValStOut[A,B] = NodeOut[A,ValSt[B]]

  type IconImageF = Int ⇒ IO[Option[Image]]
}

// vim: set ts=2 sw=2 et:
