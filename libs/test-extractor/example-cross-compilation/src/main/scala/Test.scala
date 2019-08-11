object Test {
  def f(x: Int)(implicit y: Int) = x + y
  implicit val i = 1
  f(1)
}
