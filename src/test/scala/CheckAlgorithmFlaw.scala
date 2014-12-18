import hometask.treecut.{Node, LeafNode, CutTree}
import org.scalatest.{Matchers, FlatSpec}

class CheckAlgorithmFlaw extends FlatSpec with Matchers {

  /*
           10(1)
          /    \
       -100(2)  -20(3)
      /    \
    75(5) -200(4)
  */


  "(test1) Total weight" should "be -10 after first cut" in {

    val node5 = LeafNode(5, 75)
    val node4 = LeafNode(4, -100)
    val node3 = LeafNode(3, -20)
    val node2 = Node(2, -200, Some(node5), Some(node4))
    val node1 = Node(1, 10, Some(node2), Some(node3))

    val rs = new CutTree(node1).sortCut(1)

    rs.newWeight should be(-10)
  }

  /*
           10(1)
          /    \
       -1(2)  100(3)
      /    \       \
    75(5) -200(4) -20(6)
  */

  "(test2) Total weight" should "be 164 after first cut" in {

    val node6 = LeafNode(6, -20)
    val node5 = LeafNode(5, 75)
    val node4 = LeafNode(4, -200)
    val node3 = Node(3, 100, None, Some(node6))
    val node2 = Node(2, -1, Some(node5), Some(node4))
    val node1 = Node(1, 10, Some(node2), Some(node3))

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

  "(test3) Total weight" should "be 90 after first cut" in {

    val node6 = LeafNode(6, -20)
    val node5 = LeafNode(5, -75)
    val node4 = LeafNode(4, -200)
    val node3 = Node(3, 100, None, Some(node6))
    val node2 = Node(2, -1, Some(node5), Some(node4))
    val node1 = Node(1, 10, Some(node2), Some(node3))

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

  "(test4) Total weight" should "be 110 after second cut" in {

    val node6 = LeafNode(6, -20)
    val node5 = LeafNode(5, -75)
    val node4 = LeafNode(4, -200)
    val node3 = Node(3, 100, None, Some(node6))
    val node2 = Node(2, -1, Some(node5), Some(node4))
    val node1 = Node(1, 10, Some(node2), Some(node3))

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

  "(test5) Total weight" should "be 1 after second cut" in {

    val node3 = LeafNode(3, -2)
    val node2 = Node(2, 6, Some(node3), None)
    val node1 = Node(1, -5, Some(node2), None)

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

  "(test6) Total weight" should "be 2 after third cut" in {


    val node6 = LeafNode(6, -8)
    val node5 = Node(5, -2, Some(node6), None)
    val node4 = LeafNode(4, -3)
    val node3 = Node(3, 3, Some(node4), Some(node5))
    val node2 = Node(2, 3, Some(node3), None)
    val node1 = Node(1, -5, Some(node2), None)

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

  "(test7) Total weight" should "be 5 after second cut(positive removal)" in {

    val node4 = LeafNode(4, -3)
    val node3 = LeafNode(3, -2)
    val node2 = Node(2, 1, Some(node3), Some(node4))
    val node1 = Node(1, 5, Some(node2), None)

    val rs = new CutTree(node1).sortCut(1)

    rs.newWeight should be(5)
  }

  /*
            -5(1)
  */

  "(test8) Total weight" should "be -5 if 0 maxCuts specified" in {

    val node = LeafNode(1, -5)

    val rs = new CutTree(node).sortCut(0)

    rs.newWeight should be(-5)
  }


  /*
              1(1)
              /       \
            1(2)       1(3)
           /   \       /     \
       -5(5)  -6(4)  -5(8)   -6(7)
  */

  "(test9) Total weight" should "be 1 after second cut(double positive removal)" in {

    val node5 = LeafNode(5, -5)
    val node4 = LeafNode(4, -6)
    val node8 = LeafNode(8, -5)
    val node7 = LeafNode(7, -6)
    val node2 = Node(2, 1,Some(node5), Some(node4))
    val node3 = Node(3, 1,Some(node8), Some(node7))
    val node1 = Node(3, 1,Some(node2), Some(node3))

    val rs = new CutTree(node1).sortCut(2)

    rs.newWeight should be(1)
  }


}
