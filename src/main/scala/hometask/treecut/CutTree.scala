package hometask.treecut


sealed trait Node {
  val id: Int
  val weight: Int

  def isEmpty: Boolean

  def removeNode(n: Node): Node

  def size: Int = fold2(0)((s1, s2, _) => s1 + s2 + 1)

  def fullWeight: Int = fold2(0)((w1, w2, cw) => w1 + w2 + cw.weight)

  def fold2[B](z: B)(f: (B, B, Node) => B): B
}

case object EmptyNode extends Node {
  val id: Int = 0
  val weight: Int = 0

  def removeNode(n: Node): Node = EmptyNode

  def fold2[B](z: B)(f: (B, B, Node) => B): B = z

  def isEmpty = true

}

case class NonEmptyNode(id: Int, weight: Int, left: Node, right: Node) extends Node {
  def removeNode(n: Node): Node = if (n.isEmpty) n
  else if (n.id == id) EmptyNode
  else this.copy(left = left.removeNode(n), right = right.removeNode(n))

  def fold2[B](z: B)(f: (B, B, Node) => B): B = f(left.fold2(z)(f), right.fold2(z)(f), this)

  def isEmpty = false
}

class CutTree(var root: Node) {

  case class CutStats(initialWeight: Int, newWeight: Int, removals: Int)

  def removeSubtree(n: Node): Unit = root = root.removeNode(n)

  def sortCut(maxCuts: Int): CutStats = {
    type CutAction = (Node, Int)
    type CutActions = List[CutAction]

    def getNodeWeightAndCutActions(n: Node): (Int, CutActions) = n match {
      case EmptyNode => (0, Nil)
      case NonEmptyNode(_, w, l, r) =>
        val (lw, leftWeights) = getNodeWeightAndCutActions(l)
        val (rw, rightWeights) = getNodeWeightAndCutActions(r)
        val newWeight = w + lw + rw
        if (newWeight < 0) (0, List((n, newWeight)))
        else (newWeight, leftWeights ++ rightWeights)
    }

    val (weight, actions) = getNodeWeightAndCutActions(root)
    val selectedActions = actions.sorted(Ordering.by[CutAction, Int] { case (n, w) => w}.reverse).take(maxCuts)
    val newWeight = selectedActions.foldLeft(weight) { case (acc, (n, w)) =>
      removeSubtree(n)
      acc - w
    }
    CutStats(weight, newWeight, selectedActions.length)
  }
}

