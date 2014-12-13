import hometask.treecut.{LeafNode, CutTree, EmptyNode, NonEmptyNode}
import org.scalatest.{Matchers, FlatSpec}

class CheckAlgorithmFlaw extends FlatSpec with Matchers {

  /*
           10(1)
          /    \
       -100(2)  -20(3)
      /    \
    75(5) -200(4)
  */


  "Total weight" should "be 0 after first cut" in {

    val node5 = LeafNode(5, 75)
    val node4 = LeafNode(4, -100)
    val node3 = LeafNode(3, -20)
    val node2 = NonEmptyNode(2, -200, node5, node4)
    val node1 = NonEmptyNode(1, 10, node2, node3)

    val rs = new CutTree(node1).sortCut(1)

    rs.newWeight should be(0)
  }

  /*
           10(1)
          /    \
       -1(2)  100(3)
      /    \       \
    75(5) -200(4) -20(6)
  */

  "Total weight" should "be 164 after first cut" in {

    val node6 = LeafNode(6, -20)
    val node5 = LeafNode(5, 75)
    val node4 = LeafNode(4, -200)
    val node3 = NonEmptyNode(3, 100, EmptyNode, node6)
    val node2 = NonEmptyNode(2, -1, node5, node4)
    val node1 = NonEmptyNode(1, 10, node2, node3)

    val rs = new CutTree(node1).sortCut(1)

    rs.newWeight should be(164)
  }


  /*
         10(1)
        /    \
     -1(2)  100(3)
    /    \      \
 -75(5) -200(4) -20(6)
  */

  "Total weight" should "be 90 after first cut" in {

    val node6 = LeafNode(6, -20)
    val node5 = LeafNode(5, -75)
    val node4 = LeafNode(4, -200)
    val node3 = NonEmptyNode(3, 100, EmptyNode, node6)
    val node2 = NonEmptyNode(2, -1, node5, node4)
    val node1 = NonEmptyNode(1, 10, node2, node3)

    val rs = new CutTree(node1).sortCut(1)

    rs.newWeight should be(90)
  }

  /*
            10(1)
           /    \
        -1(2)  100(3)
       /    \       \
   -75(5) -200(4) -20(6)
  */

  "Total weight" should "be 110 after second cut" in {

    val node6 = LeafNode(6, -20)
    val node5 = LeafNode(5, -75)
    val node4 = LeafNode(4, -200)
    val node3 = NonEmptyNode(3, 100, EmptyNode, node6)
    val node2 = NonEmptyNode(2, -1, node5, node4)
    val node1 = NonEmptyNode(1, 10, node2, node3)

    val rs = new CutTree(node1).sortCut(2)

    rs.newWeight should be(110)
  }

  /*
            -5(1)
             /
           6(2)
           /
         -2(3)
  */

  "Total weight" should "be 1 after second cut" in {

    val node3 = LeafNode(3, -2)
    val node2 = NonEmptyNode(2, 6, node3, EmptyNode)
    val node1 = NonEmptyNode(1, -5, node2, EmptyNode)

    val rs = new CutTree(node1).sortCut(2)

    rs.newWeight should be(1)
  }


  /*
           -5(1)
          /
        3(2)
       /
     3(3)
    /    \
  -3(4) -2(5)
         /
      -8(6)
  */

  "Total weight" should "be 2 after third cut" in {


    val node6 = LeafNode(6, -8)
    val node5 = NonEmptyNode(5, -2, node6, EmptyNode)
    val node4 = LeafNode(4, -3)
    val node3 = NonEmptyNode(3, 3, node4, node5)
    val node2 = NonEmptyNode(2, 3, node3, EmptyNode)
    val node1 = NonEmptyNode(1, -5, node2, EmptyNode)

    val rs = new CutTree(node1).sortCut(2)

    rs.newWeight should be(1)
  }


  /*
              5(1)
              /
            1(2)
           /   \
         -2(3) -3(4)
  */

  "Total weight" should "be 5 after second cut(positive removal)" in {

    val node4 = LeafNode(4, -3)
    val node3 = LeafNode(3, -2)
    val node2 = NonEmptyNode(2, 1, node3, node4)
    val node1 = NonEmptyNode(1, 5, node2, EmptyNode)

    val rs = new CutTree(node1).sortCut(1)

    rs.newWeight should be(5)
  }

  /*
            -5(1)
*/

  "Total weight" should "be -5 if 0 maxCuts specified" in {

    val node = LeafNode(1, -5)

    val rs = new CutTree(node).sortCut(0)

    rs.newWeight should be(-5)
  }


}
