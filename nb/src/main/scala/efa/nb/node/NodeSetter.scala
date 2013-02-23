package efa.nb.node

import efa.react.Out
import scalaz._, Scalaz._, scalaz.effect.{IO, IORef}

private[node] final class NodeSetter (
  val out: IORef[Out[NbNode]],
  val node: IORef[Option[NbNode]]
) {
  def setNode: Out[NbNode] = n ⇒ node.write(Some(n)) >> adjust

  def setOut: Out[Out[NbNode]] = o ⇒ out.write(o) >> adjust

  private def adjust: IO[Unit] = for {
    o ← out.read
    n ← node.read
    _ ← n map o orZero
  } yield ()
}

object NodeSetter {

  val apply: IO[NodeSetter] = ^(
    IO.newIORef((_: NbNode) ⇒ IO.ioUnit),
    IO.newIORef(none[NbNode])
  )(new NodeSetter(_, _))

  def out (out: Out[NbNode]): IO[NodeSetter] = for {
    ns ← apply
    _  ← ns setOut out
  } yield ns
}

// vim: set ts=2 sw=2 et:
