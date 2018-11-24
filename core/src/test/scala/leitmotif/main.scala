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
        case node @ El(name, _, _, _) =>
          val name1 = if (headline >= 1 && name == "h1") "h2" else name
          node.copy(name = name1)
      }
    } yield ()

  def subCountInc: LmS[MainS, Unit] =
    Leitmotif.modifyS(_.lens(_.sub.count).modify(_ + 1))

  def subCountClass: LmS[MainS, Unit] =
    for {
      env <- Leitmotif.ask
      _ <- Leitmotif.modifyEl {
        case node @ El(_, _, _, ElMeta.Regular()) =>
          val count = env.sub.count
          node.copy(attrs = Attrs(Map("class" -> s"sub-$count")))
        case a => a
      }
    } yield ()

  def div(tail: Tree*): Tree =
    Leitmotif.node(Lm.plain("div"))(tail: _*)


  def tree: Tree =
    Leitmotif.node(
      Lm.plain("section").path(recordHeadline).sub(subCountClass)
    )(
      Leitmotif.node(Lm.plain("h1").path(adaptHeadline))(
        div(div(div()), div(div()))
      )
    )

  def test1 = {
    val result = Compile(Env(PathEnv(), SubEnv(0)), MainS(Path(0), Sub(0)))(tree)
    val (_, _, tree1) = result.value
    assert(tree1.head.node.attr("class") == Some("sub-7"))
    assert(tree1.tail.value.headOption.map(_.head.node.name) == Some("h2"))
  }

  def tests = Tests("foo" - test1)
}
