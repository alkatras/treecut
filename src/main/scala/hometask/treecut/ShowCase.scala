package hometask.treecut

object ShowCase extends App {

  val generator = TreeGenerator(
    maxNodes = 1000,
    weightMin = -1000,
    weightMax = 1000
  )

  for (t <- generator.trees.filter(_.size > 100).take(10)) {
    val cutTree = new CutTree(t)
    cutTree.sortCut(100)
    println(s"Before: weight  ${t.fullWeight},size ${t.size}, tree  ${t} ")
    println(s"After: weight  ${cutTree.root.fullWeight}, size ${cutTree.root.size},tree  ${cutTree.root}")
    println("--------------------------------------------------------------")
  }


}
