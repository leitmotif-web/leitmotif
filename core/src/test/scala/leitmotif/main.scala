package leitmotif

import cats.Eval
import cats.implicits._
import monocle.macros.syntax.lens._
import utest._

import Tags._
import ReferenceData._

object Tags
{
  def div[S, L](tail: Tree[Lm[Env, S, L]]*): Tree[Lm[Env, S, L]] =
    Leitmotif.node(Lm.plain("div"))(tail: _*)

  def divWithClass[S, L](cls: String)(tail: Tree[Lm[Env, S, L]]*): Tree[Lm[Env, S, L]] =
    Leitmotif.node(Lm.withClass("div", cls))(tail: _*)
}

object ReferenceData
{
  case class Path(headline: Int)
  case class Sub(count: Int)
  case class MainS(path: Path, sub: Sub)
  case class StyleEntry()

  type Node = Lm[Env, MainS, StyleEntry]
  type Tree = leitmotif.Tree[Node]
  type LmNodeS[A] = NodeS[Env, MainS, StyleEntry, Node, A]

  def recordHeadline: LmNodeS[Unit] =
    Leitmotif.modifyS((_: MainS).lens(_.path.headline).modify(_ + 1))

  def adaptHeadline: LmNodeS[Unit] =
    for {
      headline <- Leitmotif.inspectS[Env, MainS, StyleEntry, Node, Int](_.path.headline)
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
      _ <- Leitmotif.modifyTree[Env, MainS, StyleEntry, Node](
        _.copy(tail = Eval.now(List(divWithClass("shell")(divWithClass("core")(tail: _*)))))
      )
    } yield ()

  def tree: Tree =
    Leitmotif.node(
      Lm.plain("section").pre(recordHeadline).post(subCountClass).post(shellCore).style("text-align" -> "right")
    )(
      Leitmotif.node(Lm.plain("h1").pre(adaptHeadline).post(innerShellCore))(
        div(div(div()), div(div()))
      )
    )

  def generateStyle: LmNodeS[Unit] =
    for {
      el <- Leitmotif.getEl
    } yield {
      println(el)
    }
}

object MainSpec
extends Spec
{
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

object StyleSpec
extends Spec
{
  def style = {
    val result = Compile.default(Env(PathEnv(), SubEnv(0)), MainS(Path(0), Sub(0)))(tree)
    val (_, tree1) = result.value
    val sheet = AggregateStyle(Env(PathEnv(), SubEnv(0)), MainS(Path(0), Sub(0)))(tree1)(generateStyle)
    val (_, tree2) = sheet.value
    println(tree2)
  }

  def tests = Tests("aggregate style content" - style)
}
