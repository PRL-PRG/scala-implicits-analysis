object A {
  implicit class XtensionInt(x: Int) {
    def x = 1
  }

  1.x
}
