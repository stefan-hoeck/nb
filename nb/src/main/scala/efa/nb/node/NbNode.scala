package efa.nb.node

import efa.core._, Efa._
import efa.nb.PureLookup
import efa.nb.dialog.DialogEditable
import dire._
import dire.swing.HAlign
import java.awt.Image
import java.beans.PropertyEditor
import javax.swing.Action
import org.openide.nodes.{Children, AbstractNode, Sheet, Node, NodeTransfer}
import shapeless.{HList, ::}
import scalaz._, Scalaz._, effect.{IO, IORef}

object NbNode extends NbNodeFunctions {
  def apply(isLeaf: Boolean = false): IO[NbNode] = for {
    hc  ← if (!isLeaf) NbChildren.create map (_.some)
          else IO(none)
    lkp ← PureLookup.apply
    res ← IO (new NbNode (lkp, hc))
  } yield res

  def create(ns: NodeSetter): IO[NbNode] = for {
    res ← apply(ns.isLeaf)
    _   ← ns setNode res
  } yield res

  private[node] def createIO(ns: NodeSetter) =
    create(ns).unsafePerformIO
}

trait NbNodeFunctions {
  import NodeOut.{outOnly, outImpure}

  def contextRoots[A]: NodeOut[List[String],A] = outOnly(_.contextRoots)

  def contextRootsA[A,B] (ss: List[String]): NodeOut[A,B] =
    contextRoots ∙ (_ ⇒ ss)

  def cookie[A:Unerased,B]: NodeOut[A,B] = cookies ∙ (List(_))

  def cookies[A:Unerased,B]: NodeOut[List[A],B] =
    outOnly(_.updateCookies[A])

  def cookieOption[A:Unerased,B]: NodeOut[Option[A],B] =
    cookies ∙ (_.toList)

  def desc[A,B](desc: A ⇒ String): NodeOut[A,B] =
    outImpure((n,a) ⇒ n.setShortDescription(desc(a)))

  def described[A:Described,B]: NodeOut[A,B] =
    desc(Described[A] shortDesc _ v)

  def destroy[A]: NodeOut[A,A] = destroyOption ∙ (_.some)

  def destroyEs[A,B] (f: A ⇒ State[B,Unit]): NodeOut[A,ValSt[B]] =
    destroy map (f(_).success)

  def destroyP[F[_],P,C,Path <: HList](implicit p: ParentL[F,P,C,Path])
    : NodeOut[C :: Path,ValSt[P]] =
    destroy map p.deleteV

  def destroyOption[A]: NodeOut[Option[A],A] =
    NodeOut((outA, n) ⇒ oa ⇒ n setDestroyer oa.map(outA))

  def edit[A]: NodeOut[A,A] = editOption ∙ (_.some)

  def editOption[A]: NodeOut[Option[A],A] =
    NodeOut((outA, n) ⇒ oa ⇒ n setEditor oa.map(outA))

  def editE[A,B](implicit D: Editable[A,B]): NodeOut[A,B] =
    edit collectIO D.edit

  def editS[A,B,C] (f: B ⇒ State[C,Unit])
    (implicit D: Editable[A,B]): NodeOut[A,ValSt[C]] =
    editE map (f(_).success)

  def editP[F[_],P,C:Equal,Path <: HList]
    (implicit D: Editable[C :: Path, C],
      p: ParentL[F,P,C,Path]): NodeOut[C :: Path,ValSt[P]] =
    editE withIn p.updateV

  def iconBase[B]: NodeOut[String,B] =
    outImpure(_ setIconBaseWithExtension _)

  def iconBaseA[A,B] (s: String): NodeOut[A,B] = iconBase ∙ (_ ⇒ s)

  def iconImage[A,B] (f: A ⇒ IconImageF): NodeOut[A,B] = 
    NodeOut((_,n) ⇒ a ⇒ n setIconImage Some(f(a)))

  def preferredAction[A,B](f: A ⇒ Action): NodeOut[A,B] =
    NodeOut((_,n) ⇒ a ⇒ n setPreferredAction Some(f(a)))

  def preferredActionA[A,B](a: Action): NodeOut[A,B] =
    preferredAction(_ ⇒ a)
    
  def name[A,B](f: A ⇒ String): NodeOut[A,B] =
    outImpure((n,a) ⇒ n.setDisplayName(f(a)))

  def named[A:Named,B]: NodeOut[A,B] = name[A,B](Named[A] name _ v)

  def nameA[A,B](s: String): NodeOut[A,B] = name(_ ⇒ s)

  def rename[A]: NodeOut[A,String] = NodeOut((o, n) ⇒ _ ⇒ n onRename o)

  lazy val renameD: NodeOut[EndoVal[String],DisRes[String]] =
    rename withIn (_ run _)

  lazy val renameV: NodeOut[EndoVal[String],ValRes[String]] =
   renameD map (_.validation)

  def renameEs[A,B] (f: (A,String) ⇒ State[B,Unit])
  : NodeOut[(A,EndoVal[String]),ValSt[B]] = {
    type P = (A,EndoVal[String])
    renameV.contramap[P] (_._2) withIn ((p,v) ⇒ v map (f(p._1, _)))
  }

  def addNt[A]: NodeOut[(A,String),A] =
    NodeOut((o, n) ⇒ p ⇒ n addNewType (p._2, o apply p._1))

  def addNtE[A,B](implicit D: Editable[A,B])
    : NodeOut[A,B] =
    addNt[A] contramap {a: A ⇒ (a, D name a)} collectIO D.create

  def addNtS[A,B,C] (f: B ⇒ State[C,Unit])
    (implicit D: Editable[A,B])
    : NodeOut[A,ValSt[C]] = addNtE map (f(_).success)

  def addNtP[F[_],P,C,Path <: HList](c: Path ⇒ C)
    (implicit D: Editable[C :: Path, C],
      p: ParentL[F,P,C,Path]): NodeOut[Path,ValSt[P]] =
    addNtPs[F,P,C,Path](p ⇒ List(c(p)))

  //@todo: clean up code
  def addNtPs[F[_],P,C,Path <: HList](cs: Path ⇒ List[C])
    (implicit D: Editable[C :: Path, C],
      p: ParentL[F,P,C,Path]): NodeOut[Path,ValSt[P]] = {

      val dialogOut: NodeOut[C :: Path, C] = addNtE[C :: Path,C]

      val pathOut: NodeOut[Path,C] = NodeOut((o,n) ⇒ path ⇒ {
        val cOut = dialogOut run (o, n)

        cs(path) foldMap { c ⇒ cOut apply (c :: path) }
      })

      pathOut withIn p.addV
    }

  def createNtP[F[_],P,C,Path <: HList,Id:Enum:Monoid](c: Path ⇒ C)
    (implicit D: Editable[C :: Path, C],
      p: ParentL[F,P,C,Path],
      u: UniqueIdL[C,Id]): NodeOut[Path,ValSt[P]] =
    createNtPs[F,P,C,Path,Id](p ⇒ List(c(p)))

  //@todo: clean up code
  //@todo: imporve type inference. problems with inferring Id type
  def createNtPs[F[_],P,C,Path <: HList,Id:Enum:Monoid](cs: Path ⇒ List[C])
    (implicit D: Editable[C :: Path, C],
      p: ParentL[F,P,C,Path],
      u: UniqueIdL[C,Id]): NodeOut[Path,ValSt[P]] = {

      val dialogOut: NodeOut[C :: Path, C] = addNtE[C :: Path,C]

      val pathOut: NodeOut[Path,C] = NodeOut((o,n) ⇒ path ⇒ {
        val cOut = dialogOut run (o, n)

        cs(path) foldMap { c ⇒ cOut apply (c :: path) }
      })

      pathOut withIn p.addUniqueV[Id]
    }

  /**
   * All NodeOuts defined for adding new types where defined
   * will modify the existing list of new type infos. That way,
   * they are composable via monoid append.
   *
   * However, if several such modifications affect the same node,
   * The node's list of NewTypes will grow and grow. Therefore,
   * add this NodeOut before all addNt kind of NodeOuts, so the
   * list of NewTypes will be cleared before new ones are added.
   */
  def clearNt[A,B]: NodeOut[A,B] =
    NodeOut((_, n) ⇒ _ ⇒ n setNewTypes Nil)

  def booleanW[B](n: String): NodeOut[Boolean,B] =
    writeProp(n, identity, Some(_ ⇒ new BooleanEditor))

  def intW[B](n: String): NodeOut[Int,B] =
    textW(n, identity, al = HAlign.Trailing)

  def longW[B](n: String): NodeOut[Long,B] =
    textW(n, identity, al = HAlign.Trailing)

  def doubleW[B](n: String, format: Double ⇒ String): NodeOut[Double,B] =
    textW(n, identity, toString = format, al = HAlign.Trailing)

  def showW[A:Show,B](n: String): NodeOut[A,B] =
    stringW(n) ∙ (_.shows)

  def showWTrailing[A:Show,B](n: String): NodeOut[A,B] =
    textW(n, _.shows, _.shows, al = HAlign.Trailing)

  def stringW[B](n: String): NodeOut[String,B] = textW(n, identity)

  def textW[A,B:Unerased,C](
    name: String,
    toB: A ⇒ B,
    toString: A ⇒ String = (a: A) ⇒ a.toString,
    desc: A ⇒ Option[String] = (a: A) ⇒ None,
    al: HAlign = HAlign.Leading
  ): NodeOut[A,C] =
    writeProp[A,B,C](name, toB, TextEditor.read(al, toString, desc))

  def booleanRw (n: String): NodeOut[Boolean,ValRes[Boolean]] =
    rwProp[Boolean,Boolean](
      n, identity, Validators.dummy, Some((_,_) ⇒ new BooleanEditor)
    )

  def comboRw[A:Unerased] (
    as: List[A],
    n: String,
    al: HAlign = HAlign.Trailing
  ): NodeOut[A,ValRes[A]] =
    rwProp[A,A](n, identity,
      Validators.dummy, Some((_,_) ⇒ new ComboBoxEditor(as, al)))

  def intRw (n: String, v: EndoVal[Int]): NodeOut[Int,ValRes[Int]] =
   readRw[Int](n, v, al = HAlign.Trailing)

  def longRw (n: String, v: EndoVal[Long]): NodeOut[Long,ValRes[Long]] =
   readRw[Long](n, v, al = HAlign.Trailing)

  def stringRw (n: String, v: EndoVal[String])
    : NodeOut[String,ValRes[String]] =
   readRw[String](n, v, al = HAlign.Leading)

  def readRw[A:Read:Unerased](
    name: String,
    validator: EndoVal[A],
    toString: A ⇒ String = (a: A) ⇒ a.toString,
    desc: A ⇒ Option[String] = (a: A) ⇒ None,
    al: HAlign = HAlign.Leading
  ): NodeOut[A,ValRes[A]] = textRw[A](
    name, Read[A].validator >=> validator, toString, desc, al
  )

  def textRw[A:Unerased](
    name: String,
    read: Validator[String,A],
    toString: A ⇒ String = (a: A) ⇒ a.toString,
    desc: A ⇒ Option[String] = (a: A) ⇒ None,
    al: HAlign = HAlign.Leading
  ): NodeOut[A,ValRes[A]] = {
    def ed (a: A, o: Out[ValRes[A]]) = {
      val textOut: Out[String] = o ∙ (read run _ validation)
      
      TextEditor.rw(a, al, toString, desc, textOut)
    }

    rwProp[A,A](name, identity, Validators.dummy, Some(ed(_, _)))
  }

  def rwProp[A,B:Unerased](
    name: String,
    toB: A ⇒ B,
    validator: EndoVal[B],
    editor: Option[(A, Out[ValRes[B]]) ⇒ PropertyEditor]
  ): NodeOut[A,ValRes[B]] =
    NodeOut ((o, n) ⇒ a ⇒ RwProp[A,B](name, a, toB,
      validator, editor, o) >>= n.setPut)

  def writeProp[A,B:Unerased,C](
    name: String,
    toB: A ⇒ B,
    editor: Option[A ⇒ PropertyEditor]
  ): NodeOut[A,C] =
    NodeOut(
    (_, n) ⇒ a ⇒ RProp[A,B](name, a, toB, editor) >>= n.setPut
  )

  //// Private Helper classes

  private class RProp[A,B] private (
    override val getName: String,
    a: A,
    toB: A ⇒ B,
    editor: Option[A ⇒ PropertyEditor]
  )(implicit m: Unerased[B])
   extends Node.Property[B](m.clazz) {
    override def canRead = true
    override def canWrite = false
    override def setValue (a: B) {}
    override def getValue: B = toB(a)
    override def getPropertyEditor =
      editor ∘ (_ apply a) | super.getPropertyEditor
  }

  private object RProp {
    def apply[A,B:Unerased] (
      name: String,
      a: A,
      toB: A ⇒ B,
      editor: Option[A ⇒ PropertyEditor]
    ): IO[RProp[A,B]] = IO { new RProp (name, a, toB, editor) }
  }

  private class RwProp[A,B] (
    override val getName: String,
    a: A,
    toB: A ⇒ B,
    validator: EndoVal[B],
    editor: Option[(A, Out[ValRes[B]]) ⇒ PropertyEditor],
    out: Out[ValRes[B]]
  )(implicit m: Unerased[B])
   extends Node.Property[B](m.clazz) {
    override def canRead = true
    override def canWrite = true
    override def getValue: B = toB (a)

    override def setValue (b: B) {
      out apply validator(b).validation unsafePerformIO
    }

    override def getPropertyEditor =
      editor ∘ (_ apply (a, out)) | super.getPropertyEditor
  }

  private object RwProp {
    def apply[A,B:Unerased] (
      name: String,
      a: A,
      toB: A ⇒ B,
      validator: EndoVal[B],
      editor: Option[(A, Out[ValRes[B]]) ⇒ PropertyEditor],
      out: Out[ValRes[B]]
    ): IO[RwProp[A,B]] = IO { new RwProp (name, a, toB, validator, editor, out) }
  }
}

final class NbNode private (
  lkp: PureLookup,
  private[node] val hc: Option[NbChildren]
) extends PureNode (hc getOrElse Children.LEAF, lkp.l)
  with ContextActionNode {

  // *** Cookies ***
  
  def updateCookies[A:Unerased]: Out[List[A]] = lkp.set

  // *** Property Sheet ***
  private[this] lazy val set = Sheet.createPropertiesSet

  def setPut:  Out[org.openide.nodes.Node.Property[_]] = p ⇒ IO(set put p)
  
  override final protected def createSheet: Sheet = {
    val sheet = Sheet.createDefault
    sheet put set
    sheet
  }
  
  // *** Edit ***
  def setEditor: Out[Editor] = { e ⇒ 
    import org.openide.cookies.EditCookie

    def ec = (io: IO[Unit]) ⇒ new EditCookie {
      def edit() { io.unsafePerformIO }
    }

    updateCookies[EditCookie] apply (e map ec toList)
  }

  def onEdit: Out[IO[Unit]] = setEditor ∙ (_.some)

  override final def canCopy = true

  override final def canCut = true
}

// vim: set ts=2 sw=2 et:
