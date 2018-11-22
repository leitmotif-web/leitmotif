package leitmotif

import cats.implicits._
import monocle.macros.syntax.lens._
import utest._

object MainSpec
extends Spec
{
  case class Path(headline: Int)
  case class Sub(count: Int)
  case class MainS(path: Path, sub: Sub)

  type Node = Lm[MainS]
  type Tree = leitmotif.Tree[Node]

  def recordHeadline: LmS[MainS, Unit] =
    Leitmotif.modifyS((_: MainS).lens(_.path.headline).modify(_ + 1))

  def adaptHeadline: LmS[MainS, Unit] =
    for {
      headline <- Leitmotif.inspectS[MainS, Int](_.path.headline)
      _ <- Leitmotif.modifyEl {
        case node @ El.Regular(name, _, _) =>
          val name1 = if (headline >= 1 && name == "h1") "h2" else name
          node.copy(name = name1)
        case a => a
      }
    } yield ()

  def subCountInc: LmS[MainS, Unit] =
    Leitmotif.modifyS(_.lens(_.sub.count).modify(_ + 1))

  def subCountClass: LmS[MainS, Unit] =
    for {
      count <- Leitmotif.inspectS((_: MainS).sub.count)
      _ <- Leitmotif.modifyEl {
        case node @ El.Regular(_, _, _) =>
          node.copy(attrs = Attrs(Map("class" -> s"sub-$count")))
        case a => a
      }
    } yield ()

  def div(tail: Tree*): Tree =
    Leitmotif.node[MainS](Lm(El("div", Style(), Attrs(Map()))))(tail: _*)


  def tree: Tree =
    Leitmotif.node[MainS](
      Lm(El("section", Style(), Attrs(Map()))).path(recordHeadline).sub(subCountClass)
    )(
      Leitmotif.node[MainS](Lm(El("h1", Style(), Attrs(Map()))).path(adaptHeadline))(
        div(div(div()), div(div()))
      )
    )

  def test1 = {
    val result = Compile(Env(PathEnv(), SubEnv()), MainS(Path(0), Sub(0)))(tree)
    val (_, _, tree1) = result.value
    val tree2 = Render(tree1)
    println(tree2.forceAll)
    assert(1 == 1)
  }

  def tests = Tests("foo" - test1)
}
