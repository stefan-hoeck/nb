package efa.nb.dialog

import scalaz._, Scalaz._, effect._, scalacheck.ScalaCheckBinding._
import org.scalacheck._, Prop._
import efa.core._, Efa._
import scala.swing.{TextField, ComboBox}
import efa.react._, swing.GbPanel
import efa.nb.VSIn

/**
 * Simulates a panel that is used for editing a simple
 * CaseClass. This is at the same time a unit test as well
 * as a means to try out how easy it is to set up a
 * validated UI-element from reactive BuildingBlocks
 */
object DialogEditableTest extends Properties("DialogEditable") {

  property("creation") = forAll { c: Cc ⇒ 
    val res = for {
      p ← CcPanel(c)
    } yield (p.id ≟ c.id) &&
      (p.nameC.text ≟ c.name) &&
      (p.selectionC.selection.item ≟ c.selection) &&
      (p.valueC.text ≟ c.value.toString)

    res.unsafePerformIO
  }

  private def testCcp (
    c: Cc, handle: CcPanel ⇒ Unit, exp: ValRes[Cc]
  ): Prop = {
    def res = for {
      ref ← IO newIORef "".failureNel[Cc]
      pnl ← CcPanel(c)
      _   ← pnl.sin to (ref write _) runIO ()
      _   ← IO(handle(pnl))
      res ← ref.read
    } yield (exp ≟ res) :| "Exp: %s but was: %s".format(exp, res)

    res.unsafePerformIO
  }

  property("signal") = forAll { c: Cc ⇒ testCcp (c, _ ⇒ (), c.success) }

  val ccNameGen =
    ^(Arbitrary.arbitrary[Cc], Gen.identifier)(Tuple2.apply)

  property("name") = forAll(ccNameGen) { p ⇒
    val (c, s) = p
    testCcp (c, _.nameC.text = s,
      (Cc nameV s validation) map (n ⇒ c copy (name = n)))
  }

  property("value") = forAll { p: (Cc, Int) ⇒
    val (c, i) = p
    testCcp (c, _.valueC.text = i.toString,
      (Cc valueV i validation) map (n ⇒ c copy (value = n)))
  }

  val selGen = 
    ^(Arbitrary.arbitrary[Cc], Gen oneOf ("" :: Cc.selections))(Pair.apply)

  property("selection") = forAll(selGen) { p ⇒ 
    val (c, s) = p
    testCcp (c, _.selectionC.selection.item = s,
      (Cc selectionV s validation) map (n ⇒ c copy (selection = n)))
  }
}

case class Cc (id: Int, name: String, selection: String, value: Int)

object Cc {
  val nameV = Validators.notEmptyString >=> Validators.maxStringLength(100)
  val selectionV = Validators.notEmptyString
  val valueV = Validators.interval (0, 100)
  val selections = List("a","b","c")

  implicit val CcEqual: Equal[Cc] = Equal.equalA

  implicit val CcArbitrary: Arbitrary[Cc] = Arbitrary (
    ^^^(Arbitrary.arbitrary[Int],
      Gen.identifier filter (_.length <= 100),
      Gen oneOf selections,
      Gen choose (0, 100))(Cc.apply)
  )

  implicit val CcDialogEditable = new DialogEditable[Cc,Cc] {
    type Comp = CcPanel
    def component (c: Cc, isCreate: Boolean) = CcPanel(c)
    def signalIn (c: Comp) = c.sin
    override def name (c: Cc) = c.name
  }
}

class CcPanel private(cc: Cc) extends GbPanel with DialogWidgets {
  def id = cc.id
  val nameC = new TextField(cc.name)
  val valueC = new TextField(cc.value.toString)
  val selectionC = new ComboBox ("" :: Cc.selections) {
    selection.item = cc.selection
  }

  ("name" above "selection" above "value") beside
  (nameC above selectionC above valueC) add()

  def sin: VSIn[Cc] =
    id.η[VSIn] ⊛ 
    textIn(nameC, Cc.nameV) ⊛ 
    validate(selectionC)(Cc.selectionV) ⊛ 
    textIn(valueC, Cc.valueV) apply Cc.apply
}

object CcPanel {
  def apply (cc: Cc): IO[CcPanel] = IO { new CcPanel(cc) }
}

// vim: set ts=2 sw=2 et:
