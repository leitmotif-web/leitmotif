package leitmotif

import cats.Eval
import cats.implicits._
import monocle.macros.syntax.lens._
import utest._

import Tags._

object Tags
{
  def div[S](tail: Tree[Lm[Env, S]]*): Tree[Lm[Env, S]] =
    Leitmotif.node(Lm.plain("div"))(tail: _*)

  def divWithClass[S](cls: String)(tail: Tree[Lm[Env, S]]*): Tree[Lm[Env, S]] =
    Leitmotif.node(Lm.withClass("div", cls))(tail: _*)
}

object MainSpec
extends Spec
{
  case class Path(headline: Int)
  case class Sub(count: Int)
  case class MainS(path: Path, sub: Sub)

  type Node = Lm[Env, MainS]
  type Tree = leitmotif.Tree[Node]
  type LmNodeS[A] = NodeS[Env, MainS, Lm[Env, MainS], A]

  def recordHeadline: LmNodeS[Unit] =
    Leitmotif.modifyS((_: MainS).lens(_.path.headline).modify(_ + 1))

  def adaptHeadline: LmNodeS[Unit] =
    for {
      headline <- Leitmotif.inspectS[Env, MainS, Lm[Env, MainS], Int](_.path.headline)
      _ <- Leitmotif.modifyEl {
        case node @ El(name, _, _, _) =>
          val name1 = if (headline >= 1 && name == "h1") "h2" else name
          node.copy(name = name1)
      }
    } yield ()

  def subCountInc: LmNodeS[Unit] =
    Leitmotif.modifyS(_.lens(_.sub.count).modify(_ + 1))

  def subCountClass: LmNodeS[Unit] =
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

  def shellCore: LmNodeS[Unit] =
    Leitmotif.modifyTree(a => divWithClass("shell")(divWithClass("core")(a)))

  def innerShellCore: LmNodeS[Unit] =
    for {
      tail <- Leitmotif.tail
      _ <- Leitmotif.modifyTree[Env, MainS, Lm[Env, MainS]](
        _.copy(tail = Eval.now(List(divWithClass("shell")(divWithClass("core")(tail: _*)))))
      )
    } yield ()

  def tree: Tree =
    Leitmotif.node(
      Lm.plain("section").pre(recordHeadline).post(subCountClass).post(shellCore)
    )(
      Leitmotif.node(Lm.plain("h1").pre(adaptHeadline).post(innerShellCore))(
        div(div(div()), div(div()))
      )
    )

  def reference = {
    val result = Compile.default(Env(PathEnv(), SubEnv(0)), MainS(Path(0), Sub(0)))(tree)
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

  def tests = Tests("adapt headline to section nesting, count children, inject shell/core" - reference)
}
