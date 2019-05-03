package leitmotif

import cats.Eval
import cats.data.{RWS, IRWST}
import cats.free.Cofree
import cats.implicits._
import scalatags.Text.all._

object RenderText
{
  def node[S]: El => String = {
    case El(name, _, attrs, ElMeta.Regular()) =>
      s"$name ${attrs.attrs.getOrElse("class", "")}"
    case El(_, _, _, ElMeta.Pseudo()) =>
      "pseudo"
  }

  def level[E, S](prefix: String)(tree: Tree[Lm[E, S]]): Eval[List[String]] =
    for {
      tail <- tree.tail
      sub <- tail.flatTraverse(level(s"$prefix- "))
    } yield s"$prefix${node(tree.head.node)}" :: sub
}

object RenderScalaTags
{
  def node[S]: El => Tag = {
    case El(n, _, attrs, ElMeta.Regular()) =>
      tag(n)(cls := attrs.attrs.getOrElse("class", ""))
    case El(_, _, _, ElMeta.Pseudo()) =>
      div()
  }

  def level[E, S](prefix: String)(tree: Tree[Lm[E, S]]): Eval[Tag] =
    for {
      tail <- tree.tail
      sub <- tail.traverse(level(s"$prefix- "))
    } yield node(tree.head.node).apply(sub)
}

object Render
{
  def apply[E, S](tree: Tree[Lm[E, S]]): Tree[El] =
    tree.map(_.node)

  def text[E, S](tree: Tree[Lm[E, S]]): String =
    RenderText.level("")(tree).value.mkString("\n")

  def scalatags[E, S](tree: Tree[Lm[E, S]]): Tag =
    RenderScalaTags.level("")(tree).value
}

object Leitmotif
{
  def node[E, S](head: Lm[E, S])(tail: Tree[Lm[E, S]]*): Tree[Lm[E, S]] =
    Cofree(head, Eval.now(tail.toList))

  def inspectS[E, S, N, A](f: S => A): NodeS[E, S, N, A] =
    RWS.inspect(f compose NodeS.sLens.get)

  def modifyS[E, S, N](f: S => S): NodeS[E, S, N, Unit] =
    RWS.modify(NodeS.sLens.modify(f))

  def modifyEl[E, S](f: El => El): NodeS[E, S, Lm[E, S], Unit] =
    RWS.modify(NodeS.nodeLens.modify(f))

  def inspectTreeF[E, S, N, A](f: Tree[N] => Eval[A]): NodeS[E, S, N, A] =
    IRWST.inspectF(f compose NodeS.treeLens.get)

  def modifyTree[E, S, N](f: Tree[N] => Tree[N]): NodeS[E, S, N, Unit] =
    RWS.modify(NodeS.treeLens.modify(f))

  def el[E, S]: NodeS[E, S, Lm[E, S], El] =
    RWS.inspect(NodeS.nodeLens.get)

  def ask[E, S, N]: NodeS[E, S, N, E] =
    RWS.ask[E, Vector[String], NodeState[S, N]]

  def tail[E, S, N]: NodeS[E, S, N, List[Tree[N]]] =
    inspectTreeF(_.tail)
}
