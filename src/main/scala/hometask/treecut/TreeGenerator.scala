package hometask.treecut

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen._

import scala.util.Random

case class TreeGenerator(maxNodes: Int, weightMin: Int, weightMax: Int) {

  private def nextTree: Node = {
    val rnd = Random
    def iter(size: Int): Node = {
      if (size == maxNodes || rnd.nextInt(3) == 1) EmptyNode
      else {
        val weight = weightMin + rnd.nextInt(Math.abs(weightMax - weightMin))
        val left = iter(size + 1)
        val right = iter(size + left.size + 1)
        NonEmptyNode(size + 1, weight, left, right)
      }
    }
    iter(0)
  }

  def trees: Stream[Node] = Stream.cons(nextTree, trees)


}