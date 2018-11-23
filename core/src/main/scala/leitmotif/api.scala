package leitmotif

import cats.Eval
import cats.data.RWS
import cats.free.Cofree
import cats.implicits._

object RenderText
{
  def node[S]: El => String = {
    case El.Regular(name, _, attrs) =>
      s"$name ${attrs.attrs.getOrElse("class", "")}"
    case El.Pseudo() =>
      "pseudo"
  }

  def level[S](prefix: String)(tree: Tree[Lm[S]]): Eval[List[String]] =
    tree.tail.flatMap(_.flatTraverse(level(s"$prefix- "))).map(s"$prefix${node(tree.head.node)}" :: _)
}

object Render
{
  def apply[S](tree: Tree[Lm[S]]): Tree[El] =
    tree.map(_.node)

  def text[S](tree: Tree[Lm[S]]): String =
    RenderText.level("")(tree).value.mkString("\n")
}

object Leitmotif
{
  def node[S](head: Lm[S])(tail: Tree[Lm[S]]*): Tree[Lm[S]] =
    Cofree(head, Eval.now(tail.toList))

  def inspectS[S, A](f: S => A): LmS[S, A] =
    RWS.inspect { case (s, _) => f(s) }

  def modifyS[S](f: S => S): LmS[S, Unit] =
    RWS.modify { case (s, el) => (f(s), el) }

  def modifyEl[S](f: El => El): LmS[S, Unit] =
    RWS.modify { case (s, el) => (s, f(el)) }

  def el[S]: LmS[S, El] =
    RWS.inspect(_._2)
}
