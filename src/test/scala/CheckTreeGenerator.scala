import hometask.treecut.TreeGenerator
import hometask.treecut.Node
import org.scalatest.{FlatSpec, Matchers}

class CheckTreeGenerator extends FlatSpec with Matchers {

  "Generated tree" should "be empty if max nodes = 0" in {
    TreeGenerator(0, -100, 100).trees.head.isEmpty should be(true)
  }

  "Generated tree" should "be empty if weightMin greater than weightMax" in {
    TreeGenerator(0, 100, -100).trees.head.isEmpty should be(true)
  }

  "Generated trees" should "at least one non empty" in {
    getSmallTrees.exists(!_.isEmpty) should be(true)
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