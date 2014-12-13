import hometask.treecut.{Node, LeafNode}
import org.scalatest.{Matchers, FlatSpec}

class NodeSpec extends FlatSpec with Matchers {


  /*
            1(1)
           /  \
         2(2) 3(3)
  */
  val n3 = LeafNode(2, 2)
  val n2 = LeafNode(3, 3)
  val simpleTree = Node(1, 1, Some(n3), Some(n2))


  "Simple tree full weight" should "be 6" in {
    simpleTree.fullWeight should be(6)
  }

  "Simple tree root weight" should "be 1" in {
    simpleTree.weight should be(1)
  }

  "Simple tree size" should "be 3" in {
    simpleTree.size should be(3)
  }

  "Leaf size" should "be 1" in {
    n2.size should be(1)
  }


  "Simple tree fold2" should "be 10" in {
    simpleTree.fold2(-1)((l, r, n) => l + r + n.weight * n.weight) should be(10)
  }

  /*
          1(1)
         /
       2(2)
  */

  "Tree after remove of node with id=3 " should "be 1-2 tree" in {
    simpleTree.removeDescendant(n2) should be(Node(1, 1, Some(n3), None))
  }

  "Tree after remove of root node " should "remain the same" in {
    simpleTree.removeDescendant(simpleTree) should be(simpleTree)
  }


}
