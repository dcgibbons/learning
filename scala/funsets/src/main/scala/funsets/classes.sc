object insets {
  println("welcome to the Scala worksheet")
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}