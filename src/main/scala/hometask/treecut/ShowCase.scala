package hometask.treecut

object ShowCase extends App {

  val generator = TreeGenerator(
    maxNodes = 1000,
    weightMin = -1000,
    weightMax = 1000
  )

  for (t <- generator.trees.filter(_.size > 10).take(10)) {
    val cutTree = new CutTree(t)
    val stats = cutTree.cut(100)
    println(s"Before: weight  ${stats.initialWeight},size ${t.size}")
    println(s"After: weight  ${stats.newWeight}, size ${cutTree.root.size}, removed ${stats.removals}")
    println("--------------------------------------------------------------")
  }



}
