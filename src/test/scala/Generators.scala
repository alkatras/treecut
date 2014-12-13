import hometask.treecut.{CutTree, Node}
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}

case class Generators(maxNodes: Int, weightMin: Int, weightMax: Int) {

  def emptys: Gen[Option[Node]] = const(None)

  def nonEmptys(size: Int): Gen[Option[Node]] = for {weight <- choose(weightMin, weightMax)
                                                     left <- genTree(size + 1)
                                                     right <- genTree(size + left.map(_.size).getOrElse(0) + 1)
  } yield Some(Node(size + 1, weight, left, right))

  def genTree(size: Int): Gen[Option[Node]] = if (size >= maxNodes) emptys
  else Gen.frequency((1, emptys), (4, nonEmptys(size)))

  lazy val trees: Gen[Node] = genTree(0).retryUntil(!_.isEmpty).map(_.get).retryUntil(_.size > 1)

  lazy val cutTrees: Gen[CutTree] = trees.map(new CutTree(_))


  implicit lazy val arbCutTree: Arbitrary[CutTree] = Arbitrary(cutTrees)


}