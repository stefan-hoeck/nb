package efa.nb.node

import efa.react._
import scalaz._, Scalaz._, effect.IO

case class NodeOut[-A,+B](run: (Out[B], NbNode) ⇒ Out[A]) {
  def andThen[C](c: NodeOut[B,C]): NodeOut[A,C] = 
    NodeOut[A,C]((oc,n) ⇒ run(c run (oc, n), n))

  def collect[C] (f: B ⇒ Option[C]): NodeOut[A,C] = collectIO(f ∘ (IO(_)))

  def collectIO[C] (f: B ⇒ IO[Option[C]]): NodeOut[A,C] = contramapM(oc ⇒
    b ⇒ f(b) >>= (_ map oc orZero)
  )

  def contramap[C] (f: C ⇒ A): NodeOut[C,B] =
    mapM (_ compose f)

  def contramapIO[C] (f: C ⇒ IO[A]): NodeOut[C,B] =
    mapM (oa ⇒ c ⇒ f(c) >>= oa)

  def contramapM[C](f: Out[C] ⇒ Out[B]): NodeOut[A,C] =
    NodeOut((oc, n) ⇒ run(f(oc), n))

  def eet(n: NbNode): EET[A,B] = eTrans(n)

  def eTrans[R[+_]:Reactive](n: NbNode): ETrans[R,A,B] = RTrans(ra ⇒ 
    for {
      src ← Events.srcC[B]
      _   ← Reactive[R] --> (ra, run(src.fire, n))
    } yield src
  )

  def map[C] (f: B ⇒ C): NodeOut[A,C] = collect (f ∘ Some.apply)

  def mapIO[C] (f: B ⇒ IO[C]): NodeOut[A,C] =
    collectIO(f ∘ (_ ∘ Some.apply))

  def mapM[C](f: Out[A] ⇒ Out[C]): NodeOut[C,B] =
    NodeOut((ob,n) ⇒ f(run(ob,n)))

  /**
   * Merges two node outs. The effects of this NodeOut will
   * happen before the effects of that.
   */
  def merge[C<:A,D>:B] (that: ⇒ NodeOut[C,D]): NodeOut[C,D] = 
    NodeOut((od,n) ⇒ (run(od,n): Out[C]) ⊹ that.run(od,n))

  def set(n: NbNode): SET[A,B] = eTrans(n)

  def withIn[C<:A,D](f: (C,B) ⇒ D): NodeOut[C,D] =
    NodeOut[C,D]((od, n) ⇒ c ⇒ {
      val ob: Out[B] = b ⇒ od(f(c,b))

      run(ob, n)(c)
    }
  )

  def ∘[C] (f: B ⇒ C): NodeOut[A,C] = map(f)

  def ∙[C] (f: C ⇒ A): NodeOut[C,B] = contramap(f)
}

trait NodeOutInstances {

  implicit def NodeOutFunctor[R] =
    new Functor[({type λ[α]=NodeOut[R,α]})#λ] {
      def map[A,B] (na: NodeOut[R,A])(f: A ⇒ B) = na ∘ f
    }

  implicit def NodeOutContravariant[R] =
    new Contravariant[({type λ[α]=NodeOut[α,R]})#λ] {
      def contramap[A,B] (na: NodeOut[A,R])(f: B ⇒ A) = na ∙ f
    }

  implicit def NodeOutMonoid[A,B] = new Monoid[NodeOut[A,B]] {
    val zero: NodeOut[A,B] = NodeOut((ob,n) ⇒ ∅[Out[A]])
    def append (a: NodeOut[A,B], b: ⇒ NodeOut[A,B]) = a merge b
  }
}

trait NodeOutFunctions {
  def point[A]: NodeOut[A,A] = NodeOut[A,A]((oa,_) ⇒ oa)

  def outOnly[A](f: NbNode ⇒ Out[A]): NodeOut[A,Nothing] =
    NodeOut((_,n) ⇒ f(n))

  def outImpure[A](f: (NbNode, A) ⇒ Unit): NodeOut[A,Nothing] =
    outOnly(n ⇒ a ⇒ IO(f(n,a)))
}

object NodeOut extends NodeOutInstances with NodeOutFunctions

// vim: set ts=2 sw=2 et:
