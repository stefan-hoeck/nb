package efa.nb.node

import dire.Out
import scalaz._, Scalaz._, scalaz.effect.{IO, IORef}

private[node] final class NodeSetter (
  val isLeaf: Boolean,
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

  def apply(isLeaf: Boolean): IO[NodeSetter] = ^(
    IO.newIORef((_: NbNode) ⇒ IO.ioUnit),
    IO.newIORef(none[NbNode])
  )(new NodeSetter(isLeaf, _, _))

  def out(out: Out[NbNode], isLeaf: Boolean): IO[NodeSetter] = for {
    ns ← apply(isLeaf)
    _  ← ns setOut out
  } yield ns
}

// vim: set ts=2 sw=2 et:
