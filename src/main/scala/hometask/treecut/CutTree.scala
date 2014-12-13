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

    def merge(left: CutActions, right: CutActions): CutActions = (left, right) match {
      case (Nil, _) => right
      case (_, Nil) => left
      case ((nl, lw) :: ls, (nr, rw) :: rs) =>
        if (lw < rw) (nl, lw) :: merge(ls, right)
        else (nr, rw) :: merge(left, rs)
    }

    val empty = (Nil, 0)
    val (actions, initialWeight) = if (maxCuts == 0) empty
    else root.fold2[(CutActions, Int)](empty) {
      case ((lca, lw), (rca, rw), n) =>
        val subtreeWeight = n.weight + lw + rw
        val childActions = merge(lca, rca) take maxCuts
        val childActionsWeight = childActions.foldLeft(0) { case (acc, (_, w)) => acc + w}
        if (subtreeWeight < 0 && subtreeWeight < childActionsWeight)
          (List((n, subtreeWeight)), subtreeWeight)
        else (childActions, subtreeWeight)
    }

    for ((n, _) <- actions) {
      removeSubtree(n)
    }
    CutStats(initialWeight, root.fullWeight, actions.length)
  }
}
