import hometask.treecut._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scala.util.Random


class CheckUnboundedTreeCut extends PropSpec with PropertyChecks with Matchers {

//    implicit override val generatorDrivenConfig =
//      PropertyCheckConfig(minSize = 1000, maxSize = 2000)

  val generators = new Generators(
    maxNodes = 80,
    weightMin = -1000,
    weightMax = 1000
  )

  import generators._

  property("maximum sortCut should return max weighted subtree") {
    forAll {
      t: CutTree =>
        val initial = t.root
        t.sortCut(maxNodes)
        val newWeight = t.root.fullWeight
        val maxWeight = getAllSubtrees(initial).maxBy(_.fullWeight).fullWeight
        newWeight should equal(maxWeight)
    }
  }

  property("removeSubtree should remove Node and subtracted weight must equals") {
    val rnd = Random
    forAll {
      t: CutTree =>
        if (!t.root.isEmpty) {
          val initial = t.root
          def find(id: Int) = t.root.fold2[Option[Node]](None)((l, r, n) => l.orElse(r).orElse(if (n.id == id) Some(n) else None))
          val id = 1 + rnd.nextInt(t.root.size)
          val node = find(id)
          t.removeSubtree(node.get)
          find(id).isEmpty should equal(true)
          (initial.fullWeight - t.root.fullWeight) should equal(node.get.fullWeight)
        }
    }
  }


  def getAllSubtrees(tree: Node): List[Node] = tree match {
    case EmptyNode => List(EmptyNode)
    case n@NonEmptyNode(_, w, l, r) =>
      val allSubtrees = for {ls <- getAllSubtrees(l)
                             rs <- getAllSubtrees(r)
                             if !ls.isEmpty || !rs.isEmpty
      } yield n.copy(left = ls, right = rs)
      EmptyNode :: n.copy(left = EmptyNode, right = EmptyNode) :: allSubtrees
  }

}
