package efa.nb.dialog

import dire._
import dire.swing._, Swing._
import efa.core._, Efa._
import org.scalacheck._, Prop._
import scalaz._, Scalaz._, effect._, scalacheck.ScalaCheckBinding._

/**
 * Simulates a panel that is used for editing a simple
 * CaseClass. This is at the same time a unit test as well
 * as a means to try out how easy it is to set up a
 * validated UI-element from reactive BuildingBlocks
 */
object DialogEditableTest
  extends org.scalacheck.Properties("DialogEditable") 
  with dire.util.TestFunctions {

  import Cc.{CcDialogEditable ⇒ DE}
  def editSF(cc: Cc) = DE info (cc, false) map { _._2 }

  property("test_info") = forAll { c: Cc ⇒ 
    runN(SF io editSF(c), 1) ≟ List(c.success)
  }
}

case class Cc (id: Int, name: String, selection: String, value: Int)

object Cc {
  import efa.nb.Widgets._

  val nameV = Validators.notEmptyString >=> Validators.maxStringLength(100)
  val selectionV = Validators.notEmptyString
  val valueV = Validators.interval (0, 100)
  val selections = List("a","b","c")

  implicit val CcEqual: Equal[Cc] = Equal.equalA

  implicit val CcShow: Show[Cc] = Show shows { _.name }

  implicit val CcArbitrary: Arbitrary[Cc] = Arbitrary (
    ^^^(Arbitrary.arbitrary[Int],
      Gen.identifier filter (_.length <= 100),
      Gen oneOf selections,
      Gen choose (0, 100))(Cc.apply)
  )

  implicit val CcDialogEditable = DialogEditable.io1[Cc,Cc] { c ⇒ 
    for {
      name      ← TextField(text := c.name)
      value     ← TextField trailing c.value.toString
      selection ← ComboBox(selections, c.selection)

      elem = ("Name" ^^ "Value" ^^ "Selection") <>
             (name ^^ value ^^ selection)

      in = c.id.η[VSIn] ⊛
           (name.in >=> validate(nameV)) ⊛
           (selection.in >=> validate(selectionV)) ⊛
           (value.in >=> read[Int] >=> reValidate(valueV)) apply Cc.apply
    } yield (elem, in)
  }
}

// vim: set ts=2 sw=2 et:
