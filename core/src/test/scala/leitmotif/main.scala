package leitmotif

import cats.implicits._
import utest._

object MainSpec
extends Spec
{
  case class Path(headline: Int)
  case class Sub(count: Int)

  type Node = Lm[Path, Sub]
  type Tree = leitmotif.Tree[Node]

  def sectionPath: (Path, El) => (Path, El) =
    (path, node) => (path.copy(headline = path.headline + 1), node)

  def h1Path: (Path, El) => (Path, El) = {
    case (path, node @ El.Regular(name, _, _)) =>
      val name1 = if (path.headline >= 1 && name == "h1") "h2" else name
      (path, node.copy(name = name1))
    case (path, node) => (path, node)
  }

  def tree: Tree = Leitmotif.node[Path, Sub](
    Lm(El("section", Style(), Attrs())).path(sectionPath)
  )(
    Leitmotif.node[Path, Sub](Lm(El("h1", Style(), Attrs())).path(h1Path))()
  )

  def test1 = {
    val result = Compile(Context(Path(0), Sub(0)))(tree)
    val (_, tree1) = result.value
    val tree2 = Render(tree1)
    println(tree2.forceAll)
    assert(1 == 1)
  }

  def tests = Tests("foo" - test1)
}
