package hometask.treecut

import scala.util.Random

case class TreeGenerator(maxNodes: Int, weightMin: Int, weightMax: Int) {

  private def nextTree: Node = {
    val rnd = Random
    def iter(size: Int): Option[Node] = {
      if ((size >= maxNodes || rnd.nextInt(3) == 1) && size != 0) None
      else {
        val weight = weightMin + rnd.nextInt(Math.abs(weightMax - weightMin))
        val left = iter(size + 1)
        val right = iter(size + left.map(_.size).getOrElse(0) + 1)
        Some(Node(size + 1, weight, left, right))
      }
    }
    iter(0).get
  }

  def trees: Stream[Node] = Stream.cons(nextTree, trees)


}