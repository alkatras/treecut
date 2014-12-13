import hometask.treecut.{LeafNode, NonEmptyNode, EmptyNode}
import org.scalatest.{Matchers, FlatSpec}

class NodeSpec extends FlatSpec with Matchers {

  "EmptyNode isEmpty" should "be true" in {
    EmptyNode.isEmpty should be(true)
  }

  "EmptyNode size" should "be 0" in {
    EmptyNode.size should be(0)
  }

  "EmptyNode weight" should "be 0" in {
    EmptyNode.weight should be(0)
  }

  "EmptyNode fullWeight" should "be 0" in {
    EmptyNode.fullWeight should be(0)
  }

  /*
            1(1)
           /  \
         2(2) 3(3)
  */
  val n3 = LeafNode(2, 2)
  val n2 = LeafNode(3, 3)
  val simpleTree = NonEmptyNode(1, 1, n3, n2)


  "Simple tree full weight" should "be 6" in {
    simpleTree.fullWeight should be(6)
  }

  "Simple tree root weight" should "be 1" in {
    simpleTree.weight should be(1)
  }

  "Simple tree size" should "be 3" in {
    simpleTree.size should be(3)
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
    simpleTree.removeNode(n2) should be(NonEmptyNode(1, 1, n3, EmptyNode))
  }

  "Tree after remove of root node " should "be empty" in {
    simpleTree.removeNode(simpleTree) should be(EmptyNode)
  }


}
