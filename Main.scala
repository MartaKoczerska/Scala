object Main extends App {
  trait GeneralTree[A] {
  case class Leaf (val data:A) extends GeneralTree[A]
  case class Node (val data:A, val children:Seq[Node]) extends GeneralTree[A]
  }
  def size(n:Node):Int = {
  n.children.foldLeft(0)((s,c) => s + size(c)) -- trzeba zamienic na tail recursion
  }
  
  def height(n:Node):Int = {
  1+n.children.foldLeft(-1)((h,c) => h max height(c)) -- trzeba zamienic na tail recursion
  }
  }
