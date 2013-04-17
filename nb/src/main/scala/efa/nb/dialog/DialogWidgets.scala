package efa.nb.dialog

//trait DialogWidgets extends DialogWidgetsInstances with DialogWidgetsFunctions
//
//trait DialogWidgetsFunctions {
//
//  def checkBox (cb: CheckBox): VSIn[Boolean] = valIn(selectedS(cb))
//
//  def comboBox[A] (b: ComboBox[A]): VSIn[A] = valIn(itemS(b))
//
//  def doubleIn(
//    t: TextComponent, v: EndoVal[Double] = Validators.dummy[Double]
//  ): VSIn[Double] = textIn(t, v)
//
//  def intIn(
//    t: TextComponent,
//    v: EndoVal[Int] = Validators.dummy[Int]
//  ): VSIn[Int] = textIn(t, v)
//
//  def longIn(
//    t: TextComponent,
//    v: EndoVal[Long] = Validators.dummy[Long]
//  ): VSIn[Long] = textIn(t, v)
//
//  def passwordIn (
//    t: PasswordField,
//    v: EndoVal[String] = Validators.dummy[String]
//  ): VSIn[String] = validate(valIn(passwordS(t)))(v)
//
//  def stringIn(
//    t: TextComponent,
//    v: EndoVal[String] = Validators.dummy[String]
//  ): VSIn[String] = textIn(t, v)
//
//  def textIn[A:Read](
//    t: TextComponent,
//    v: EndoVal[A] = Validators.dummy[A]
//  ): VSIn[A] = validate(valIn(textS(t)))(Read[A].validator >=> v)
//
//  def textIO[A:Read](
//    t: TextComponent,
//    v: A ⇒ IO[ValRes[A]]
//  ): VSIn[A] = valIO(textIn[A](t))(v)
//
//}

// vim: set ts=2 sw=2 et:
