import hometask.treecut.TreeGenerator
import hometask.treecut.Node
import org.scalatest.{FlatSpec, Matchers}

class TreeGeneratorSpec extends FlatSpec with Matchers {


  "Generated trees" should "contain at least two different trees" in {
    getSmallTrees.distinct.size >= 2 should be(true)
  }

  "Generated trees" should "not contain tree bigger than 10" in {
    getSmallTrees.exists(_.size > 10) should be(false)
  }

  "Generated trees" should "not contain tree with node that weighs more than 100 or less than -100" in {
    getSmallTrees.exists(r => r.weight > 100 || r.weight < -100) should be(false)
  }


  def getSmallTrees: Stream[Node] = {
    TreeGenerator(10, -100, 100).trees.take(5)
  }
}