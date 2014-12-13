import hometask.treecut.{CutTree, EmptyNode, NonEmptyNode}
<<<<<<< HEAD
import org.scalatest.{Matchers, FlatSpec}

class CheckAlgorithmFlaw extends FlatSpec with Matchers {
=======
import org.scalatest.FlatSpec

class CheckAlgorithmFlaw extends FlatSpec{
>>>>>>> 2cc893ec3375bb29fe3ebab6032974002d87a73b

  /*
           10(1)
          /    \
       -100(2)  -20(3)
      /    \
    75(5) -200(4)
<<<<<<< HEAD
  */


  "Total weight" should "be 0 after first cut" in {
=======

  */

  "Total wight" should "be -10 after first cut" in {
>>>>>>> 2cc893ec3375bb29fe3ebab6032974002d87a73b

    val node5 = new NonEmptyNode(5, 75, EmptyNode, EmptyNode)
    val node4 = new NonEmptyNode(4, -100, EmptyNode, EmptyNode)
    val node3 = new NonEmptyNode(3, -20, EmptyNode, EmptyNode)
    val node2 = new NonEmptyNode(2, -200, node5, node4)
    val node1 = new NonEmptyNode(1, 10, node2, node3)

    val rs = new CutTree(node1).sortCut(1)

<<<<<<< HEAD
    rs.newWeight should be(0)
  }

=======
    assert(rs.newWeight == -10)

  }
>>>>>>> 2cc893ec3375bb29fe3ebab6032974002d87a73b
  /*
           10(1)
          /    \
       -1(2)  100(3)
      /    \       \
    75(5) -200(4) -20(6)
  */

<<<<<<< HEAD
  "Total weight" should "be 184 after first cut" in {
=======
  "Total wight" should "be 184 after first cut" in {
>>>>>>> 2cc893ec3375bb29fe3ebab6032974002d87a73b

    val node6 = new NonEmptyNode(6, -20, EmptyNode, EmptyNode)
    val node5 = new NonEmptyNode(5, 75, EmptyNode, EmptyNode)
    val node4 = new NonEmptyNode(4, -200, EmptyNode, EmptyNode)
    val node3 = new NonEmptyNode(3, 100, EmptyNode, node6)
    val node2 = new NonEmptyNode(2, -1, node5, node4)
    val node1 = new NonEmptyNode(1, 10, node2, node3)

    val rs = new CutTree(node1).sortCut(1)

<<<<<<< HEAD
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

    val node6 = new NonEmptyNode(6, -20, EmptyNode, EmptyNode)
    val node5 = new NonEmptyNode(5, -75, EmptyNode, EmptyNode)
    val node4 = new NonEmptyNode(4, -200, EmptyNode, EmptyNode)
    val node3 = new NonEmptyNode(3, 100, EmptyNode, node6)
    val node2 = new NonEmptyNode(2, -1, node5, node4)
    val node1 = new NonEmptyNode(1, 10, node2, node3)

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

    val node6 = new NonEmptyNode(6, -20, EmptyNode, EmptyNode)
    val node5 = new NonEmptyNode(5, -75, EmptyNode, EmptyNode)
    val node4 = new NonEmptyNode(4, -200, EmptyNode, EmptyNode)
    val node3 = new NonEmptyNode(3, 100, EmptyNode, node6)
    val node2 = new NonEmptyNode(2, -1, node5, node4)
    val node1 = new NonEmptyNode(1, 10, node2, node3)

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

    val node3 = new NonEmptyNode(3, -2, EmptyNode, EmptyNode)
    val node2 = new NonEmptyNode(2, 6, node3, EmptyNode)
    val node1 = new NonEmptyNode(1, -5, node2, EmptyNode)

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

    val r = NonEmptyNode(1, -5, NonEmptyNode(2, 3, NonEmptyNode(3, 3, NonEmptyNode(4, -3, EmptyNode, EmptyNode),
      NonEmptyNode(5, -2, NonEmptyNode(6, -8, EmptyNode, EmptyNode), EmptyNode)), EmptyNode), EmptyNode)

    val rs = new CutTree(r).sortCut(2)

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

    val node4 = new NonEmptyNode(4, -3, EmptyNode, EmptyNode)
    val node3 = new NonEmptyNode(3, -2, EmptyNode, EmptyNode)
    val node2 = new NonEmptyNode(2, 1, node3, node4)
    val node1 = new NonEmptyNode(1, 5, node2, EmptyNode)

    val rs = new CutTree(node1).sortCut(1)

    rs.newWeight should be(5)
  }


}
=======
    assert(rs.newWeight == 164)
  }

}
>>>>>>> 2cc893ec3375bb29fe3ebab6032974002d87a73b
