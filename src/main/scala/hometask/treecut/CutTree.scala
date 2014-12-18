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

  def cut(maxCuts: Int): CutStats = {
    type CutActions = List[Node]
    type Subtree = (Option[Node], Int, CutActions)

    val empty: Subtree = (None, 0, Nil)
    val initialWeight = root.fullWeight
    val subtrees = root.fold2(List(empty)) {
      case (lst, rst, n) =>
        val subtrees: List[Subtree] = for {(lnd, lcw, lca) <- lst
                                           (rnd, rcw, rca) <- rst
                                           newWeight = lcw + rcw + n.weight
        } yield (Some(n.copy(left = lnd, right = rnd)), newWeight, lca ++ rca)
        (None, 0, List(n)) :: subtrees
    }
    val (_, newWeight, actions) = subtrees.view.filter {
      case ((on, _, acs)) => !on.isEmpty && acs.length <= maxCuts
    }.maxBy {
      case ((_, w, _)) => w
    }

    for (n <- actions) {
      removeSubtree(n)
    }
    CutStats(initialWeight, newWeight, actions.length)
  }
}
