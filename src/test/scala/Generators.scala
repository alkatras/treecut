import hometask.treecut.{CutTree, EmptyNode, Node, NonEmptyNode}
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}

case class Generators(maxNodes: Int, weightMin: Int, weightMax: Int) {

  def emptys: Gen[Node] = const(EmptyNode)

  def nonEmptys(size: Int): Gen[Node] = for {weight <- choose(weightMin, weightMax)
                                             left <- genTree(size + 1)
                                             right <- genTree(size + left.size + 1)
  } yield NonEmptyNode(size + 1, weight, left, right)

  def genTree(size: Int): Gen[Node] = if (size == maxNodes) emptys
  else Gen.frequency((1, emptys), (4, nonEmptys(size)))

  lazy val trees: Gen[Node] = genTree(0)

  lazy val cutTrees: Gen[CutTree] = trees.map(new CutTree(_))

  implicit lazy val arbCutTree: Arbitrary[CutTree] = Arbitrary(cutTrees)


}