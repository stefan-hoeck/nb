package efa.nb.node

import efa.core.{UniqueId, Named, IntId, LongId, Parent}
import efa.react._
import org.openide.nodes.{Children, Node}
import scalaz._, Scalaz._, effect.IO

final class NbChildren private() extends Children.Keys[NodeSetter] {
  import NbChildren._

  override def createNodes (ns: NodeSetter) = Array(NbNode createIO ns)

  import scala.collection.JavaConversions._

  override protected def addNotify() {
    if (!addNotified) {
      addNotified = true
      setKeys(seq)
    }
  }

  private[node] def set (np: FullInfo): IO[Unit] = IO {
    map = np._1
    seq = np._2
    if (addNotified) setKeys (seq)
  }

  private[this] var addNotified = false
  private[this] var map: FullMap = Map.empty
  private[this] var seq: Setters = IndexedSeq.empty

  private[node] def mapAt (i: Int): IO[SetterMap] =
    IO (map get i getOrElse Map.empty)

  private[node] def children: IO[Setters] = IO (seq)
}

object NbChildren extends NbChildrenFunctions {
  val create: IO[NbChildren] = IO (new NbChildren)
}

trait NbChildrenFunctions {
  type Info[+A,+B,K] = (Map[K,A],IndexedSeq[B])

  type Setter = NodeSetter

  //Map from unique index to corresponding node
  type SetterMap = Map[Any, Setter] 

  //Sequence of index-node pair
  type Setters = IndexedSeq[Setter]

  type SetterInfo = (SetterMap, Setters)
  
  //Handles (at least part of) the Children of type A
  //Creates a (modified) sequence of nodes from a value of type A
  //and a SourceMap
  //Side effects are a possibility, since some of the nodes
  //might be adjusted during creation of the SourceSeq
  type Factory[-A,+B] = (Out[B], A, SetterMap) ⇒ IO[SetterInfo]

  //Maps and Int-Index to a OutSourceMap
  type FullMap = Map[Int, SetterMap]
 
  type FullInfo = (FullMap, Setters)  

  implicit def InfoMonoid[A,B,K] = new Monoid[Info[A,B,K]] {
    val zero: Info[A,B,K] = (Map.empty, IndexedSeq.empty)
    def append (a: Info[A,B,K], b: ⇒ Info[A,B,K]) =
      (a._1 ++ b._1, a._2 ++ b._2)
  }

  implicit def FactoryFunctor[R] =
    new Functor[({type λ[α]=Factory[R,α]})#λ] {
    def map[A,B] (nf: Factory[R,A])(f: A ⇒ B): Factory[R,B] =
      (ob,a,m) ⇒ nf (ob compose f,a,m)
  }

  implicit def FactoryContravariant[R] =
    new Contravariant[({type λ[α]=Factory[α,R]})#λ] {
    def contramap[A,B] (nf: Factory[A,R])(f: B ⇒ A): Factory[B,R] =
      (ob,a,m) ⇒ nf (ob,f(a),m)
  }

  /** Displays a single value of type `A` in a `Node`.
    *
    * If the value changes, no new `Node` is created, but
    * the existing `Node` is overwritten. This will keep it
    * expanded if it was so before.
    *
    * @tparam A The type of objects to be displayed in a `Node`
    * @tparam B The input type fired by the `Node` upon user input
    */
  def singleF[A,B] (out: NodeOut[A,B]): Factory[A,B] =
    pairsF[Unit,A,B,Id](out) ∙ { a: A ⇒ ((), a) }

  /** Displays a list of objects each in a `Node`.
    *
    * New `Node`s are created every time the sequence
    * changes, therefore this factory is usually not
    * well suited for `Node`s that have children,
    * since those will be in a collapsed state whenever
    * a new sequence is displayed, no matter what the
    * previos state of the `Node`s was.
    *
    * @tparam A The type of objects to be displayed in a `Node`
    * @tparam B The input type fired by the `Node` upon user input
    */
  def leavesF[A,B,C,F[_]:Traverse] 
    (out: NodeOut[A,B])
    (get: C ⇒ F[A]): Factory[C,B] = (ob,c, _) ⇒ {
      def setter(a: A) = create(out)(ob, a)

      get(c) traverse setter map { ss ⇒ (Map.empty, ss.toIndexedSeq) }
    }

  /** Displays a container of objects each in a `Node`.
    *
    * This function takes an implicit `efa.core.Parent` instance
    * to determine the children of `P`. Children are displayed in
    * the same order as returned by `Parent.children`.
    *
    * @tparam F  The container type in which children are stored
    * @tparam P  The parent type
    * @tparam C  The child type
    * @tparam X  The input type fired by the `Node` upon user input
    * @tparam Id Type of unique identifiers used to distinguish children
    */
  def parentF[F[_],P,C,X,Id]
    (out: NodeOut[C,X])
    (implicit U: UniqueId[C,Id], P: Parent[F,P,C]): Factory[P,X] = {
      implicit val t = P.T

      pairsF[Id,C,X,F](out) ∙ { p: P ⇒ U pairs P.children(p) }
    }

  /** Same as `parentF` but items are sorted by name before being displayed.
    *
    * @tparam F  The container type in which children are stored
    * @tparam P  The parent type
    * @tparam C  The child type
    * @tparam X  The input type fired by the `Node` upon user input
    * @tparam Id Type of unique identifiers used to distinguish children
    */
  def parentNamedF[F[_],P,C,X,Id]
    (out: NodeOut[C,X])
    (implicit U: UniqueId[C,Id],
      N: Named[C],
      P: Parent[F,P,C]): Factory[P,X] =
    pairsF[Id,C,X,List](out) ∙ { p: P ⇒ U pairs P.sortedChildren(p) }

  /** Same as `parentF` but items are sorted before being displayed.
    *
    * @tparam F  The container type in which children are stored
    * @tparam P  The parent type
    * @tparam C  The child type
    * @tparam X  The input type fired by the `Node` upon user input
    * @tparam Id Type of unique identifiers used to distinguish children
    */
  def parentSortedF[F[_],P,C,X,Id]
    (out: NodeOut[C,X])
    (sorter: (C,C) ⇒ Boolean)
    (implicit U: UniqueId[C,Id], P: Parent[F,P,C]): Factory[P,X] =
    pairsF[Id,C,X,List](out) ∙ {
      p: P ⇒ U pairs P.childrenList(p).sortWith(sorter)
    }

  def uidF[Id,A,B,C,F[_]:Traverse]
    (out: NodeOut[B,C])
    (get: A ⇒ F[B])
    (implicit U: UniqueId[B,Id]): Factory[A,C] =
    pairsF[Id,B,C,F](out) ∙ { a: A ⇒ U pairs get(a) }

  /** Displays a collection of key - value pairs in nodes.
    *
    * Existing nodes for a given key A are reused. The order in which
    * the nodes are displayed is the same as in the given data structure.
    * Note that keys must be unique, otherwise the behavior of this
    * function is undefined.
    */
  def pairsF[K,A,B,F[_]:Traverse] (out: NodeOut[A,B]): Factory[F[(K,A)],B] = 
    (oc, abs, m) ⇒ {
      def setterPair (p: (K,A)): IO[(Any,NodeSetter)] = p match {
        case (a,b) ⇒ 
          m get a cata (display(out)(oc, b), create(out)(oc, b)) strengthL a 
      }
      
      for {
        pairs ← abs traverse setterPair
        ixsq  = pairs.toIndexedSeq
      } yield (ixsq.toMap, ixsq map (_._2))
    }

  def children[A,B] (fs: Factory[A,B]*): NodeOut[A,B] = NodeOut(
    (ob, n) ⇒ a ⇒ {

      def single (p: (Factory[A,B],Int)): IO[FullInfo] = for {
        sm ← n.hc mapAt p._2
        si ← p._1 (ob, a, sm)
      } yield (Map (p._2 -> si._1), si._2)

      fs.toList.zipWithIndex foldMap single flatMap n.hc.set
    }
  )

  private def display[A,B] (no: NodeOut[A,B])(ob: Out[B], a: A)(s: NodeSetter) =
    s setOut (n ⇒ no.run(ob, n)(a)) as s

  private def create[A,B] (no: NodeOut[A,B])(ob: Out[B], a: A) =
    NodeSetter.apply >>= display(no)(ob, a)
}

// vim: set ts=2 sw=2 et:
