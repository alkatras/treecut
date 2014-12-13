package hometask.treecut

case class Node(id: Int, weight: Int, left: Option[Node], right: Option[Node]) {
  def removeDescendant(n: Node): Node = {
    def removeFromSubtree(sn: Option[Node]): Option[Node] = sn.filterNot(_.id == n.id).map(_.removeDescendant(n))
    this.copy(left = removeFromSubtree(left), right = removeFromSubtree(right))
  }

  def fold2[B](z: B)(f: (B, B, Node) => B): B = {
    def foldOnBranch(br: Option[Node]) = br.map(_.fold2(z)(f)).getOrElse(z)
    f(foldOnBranch(left), foldOnBranch(right), this)
  }

  def size: Int = fold2(0)((s1, s2, _) => s1 + s2 + 1)

  def fullWeight: Int = fold2(0)((w1, w2, cw) => w1 + w2 + cw.weight)
}

object LeafNode {
  def apply(id: Int, weight: Int) = Node(id, weight, None, None)
}

class CutTree(var root: Node) {

  case class CutStats(initialWeight: Int, newWeight: Int, removals: Int)

  def removeSubtree(n: Node): Unit = root = root.removeDescendant(n)

  //*****************************************************************************
  //  Algorithm based on modified merge sort.
  //  Run time properties.
  //  For find all remove actions:
  //  Worst case - nlog(n), example: tree where we must remove all leafs
  //  best case - n, example: tree with all positive weighted nodes
  //  removeDescendant: has linear time cost in the Node immutable data structure
  //  (may be reduced to constant in mutable)
  //  hence overall sortCut time is about n^2 in the worst case
  //*****************************************************************************
  def sortCut(maxCuts: Int): CutStats = {
    type CutActions = List[(Node, Int)]

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
        if (subtreeWeight < childActionsWeight && n.id != root.id)
          (List((n, subtreeWeight)), subtreeWeight)
        else (childActions, subtreeWeight)
    }

    for ((n, _) <- actions) {
      removeSubtree(n)
    }
    CutStats(initialWeight, root.fullWeight, actions.length)
  }
}
