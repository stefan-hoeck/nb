package efa.nb.dialog

import efa.core._, Efa._
import efa.nb.VSIn
import efa.react.{SIn, Out, sTrans, AsSignal}
import efa.react.swing.AllFunctions._
import scala.swing._
import scalaz._, Scalaz._, effect.IO

trait DialogWidgets extends DialogWidgetsInstances with DialogWidgetsFunctions

trait DialogWidgetsFunctions {
  def validate[A,B](in: VSIn[A])(v: Validator[A,B]): VSIn[B] =
    in map (_.disjunction flatMap v.run validation)

  def valIO[A,B](in: VSIn[A])(v: A ⇒ IO[ValRes[B]]): VSIn[B] =
    in mapIO (_ traverse v map (_ flatMap identity))

  def valIn[A](in: SIn[A]): VSIn[A] = in ∘ (_.success)

  def sin[A,B](a: A)(implicit S: AsSignal[A,B]): VSIn[B] = 
    valIn(sTrans in a)

  def checkBox (cb: CheckBox): VSIn[Boolean] = valIn(selectedS(cb))

  def comboBox[A] (b: ComboBox[A]): VSIn[A] = valIn(itemS(b))

  def doubleIn(
    t: TextComponent, v: EndoVal[Double] = Validators.dummy[Double]
  ): VSIn[Double] = textIn(t, v)

  def intIn(
    t: TextComponent,
    v: EndoVal[Int] = Validators.dummy[Int]
  ): VSIn[Int] = textIn(t, v)

  def longIn(
    t: TextComponent,
    v: EndoVal[Long] = Validators.dummy[Long]
  ): VSIn[Long] = textIn(t, v)

  def passwordIn (
    t: PasswordField,
    v: EndoVal[String] = Validators.dummy[String]
  ): VSIn[String] = validate(valIn(passwordS(t)))(v)

  def stringIn(
    t: TextComponent,
    v: EndoVal[String] = Validators.dummy[String]
  ): VSIn[String] = textIn(t, v)

  def textIn[A:Read](
    t: TextComponent,
    v: EndoVal[A] = Validators.dummy[A]
  ): VSIn[A] = validate(valIn(textS(t)))(Read[A].validator >=> v)

  def textIO[A:Read](
    t: TextComponent,
    v: A ⇒ IO[ValRes[A]]
  ): VSIn[A] = valIO(textIn[A](t))(v)

}

trait DialogWidgetsInstances {
  import DialogWidgets.valIn

  implicit val VSInFunctor: Functor[VSIn] =
    Functor[SIn].compose[ValRes]

  implicit val VSInApplicative: Applicative[VSIn] =
    Applicative[SIn].compose[ValRes]

  implicit def CheckBoxSIn (b: CheckBox): VSIn[Boolean] =
    valIn(selectedS(b))

  implicit def ComboBoxSIn[A] (b: ComboBox[A]): VSIn[A] =
    valIn(itemS(b))

  implicit def LabelOutString (l: Label): Out[String] = text(l)

  implicit def LabelOutShows[A:Show] (l: Label): Out[A] = text(l) ∙ (_.shows)

  implicit def PasswordSIn (b: PasswordField): VSIn[String] =
    valIn(passwordS(b))
}

object DialogWidgets extends DialogWidgets

// vim: set ts=2 sw=2 et:
