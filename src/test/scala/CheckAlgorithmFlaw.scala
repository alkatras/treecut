import hometask.treecut.{CutTree, EmptyNode, NonEmptyNode}
import org.scalatest.FlatSpec

class CheckAlgorithmFlaw extends FlatSpec{

  /*
           10(1)
          /    \
       -100(2)  -20(3)
      /    \
    75(5) -200(4)

  */

  "Total wight" should "be -10 after first cut" in {

    val node5 = new NonEmptyNode(5, 75, EmptyNode, EmptyNode)
    val node4 = new NonEmptyNode(4, -100, EmptyNode, EmptyNode)
    val node3 = new NonEmptyNode(3, -20, EmptyNode, EmptyNode)
    val node2 = new NonEmptyNode(2, -200, node5, node4)
    val node1 = new NonEmptyNode(1, 10, node2, node3)

    val rs = new CutTree(node1).sortCut(1)

    assert(rs.newWeight == -10)

  }
  /*
           10(1)
          /    \
       -1(2)  100(3)
      /    \       \
    75(5) -200(4) -20(6)
  */

  "Total wight" should "be 184 after first cut" in {

    val node6 = new NonEmptyNode(6, -20, EmptyNode, EmptyNode)
    val node5 = new NonEmptyNode(5, 75, EmptyNode, EmptyNode)
    val node4 = new NonEmptyNode(4, -200, EmptyNode, EmptyNode)
    val node3 = new NonEmptyNode(3, 100, EmptyNode, node6)
    val node2 = new NonEmptyNode(2, -1, node5, node4)
    val node1 = new NonEmptyNode(1, 10, node2, node3)

    val rs = new CutTree(node1).sortCut(1)

    assert(rs.newWeight == 164)
  }

}
