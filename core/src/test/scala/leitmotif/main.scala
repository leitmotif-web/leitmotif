package leitmotif

import cats.implicits._
import monocle.macros.syntax.lens._
import utest._

import Tags._

object Tags
{
  def div[S](tail: Tree[Lm[S]]*): Tree[Lm[S]] =
    Leitmotif.node(Lm.plain("div"))(tail: _*)

  def divWithClass[S](cls: String)(tail: Tree[Lm[S]]*): Tree[Lm[S]] =
    Leitmotif.node(Lm.withClass("div", cls))(tail: _*)
}

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
          val cls = node.attr("class").getOrElse("")
          node.copy(attrs = Attrs(Map("class" -> s"sub-$count $cls")))
        case a => a
      }
    } yield ()

  def shellCore[S]: LmS[S, Unit] =
    Leitmotif.modifyTree(a => divWithClass("shell")(divWithClass("core")(a)))

  def tree: Tree =
    Leitmotif.node(
      Lm.plain("section").pre(recordHeadline).post(subCountClass).post(shellCore)
    )(
      Leitmotif.node(Lm.plain("h1").pre(adaptHeadline))(
        div(div(div()), div(div()))
      )
    )

  def test1 = {
    val result = Compile(Env(PathEnv(), SubEnv(0)), MainS(Path(0), Sub(0)))(tree)
    val (_, tree1) = result.value
    println(Render.text(tree1))
    val cls = tree1.head.node.attr("class")
    assert(cls == Some("sub-6 shell"))
    val name =
      for {
        sub1 <- tree1.tail.value.headOption
        sub2 <- sub1.tail.value.headOption
        sub3 <- sub2.tail.value.headOption
      } yield sub3.head.node.name
    assert(name == Some("h2"))
  }

  def tests = Tests("foo" - test1)
}
