package leitmotif

import cats.Eval
import cats.data.{RWS, IRWST}
import cats.free.Cofree
import cats.implicits._

object RenderText
{
  def node[S]: El => String = {
    case El(name, _, attrs, ElMeta.Regular()) =>
      s"$name ${attrs.attrs.getOrElse("class", "")}"
    case El(_, _, _, ElMeta.Pseudo()) =>
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

  def inspectS[S, N, A](f: S => A): NodeS[S, N, A] =
    RWS.inspect(f compose NodeS.sLens.get)

  def modifyS[S, N](f: S => S): NodeS[S, N, Unit] =
    RWS.modify(NodeS.sLens.modify(f))

  def modifyEl[S](f: El => El): NodeS[S, Lm[S], Unit] =
    RWS.modify(NodeS.nodeLens.modify(f))

  def inspectTreeF[S, N, A](f: Tree[N] => Eval[A]): NodeS[S, N, A] =
    IRWST.inspectF(f compose NodeS.treeLens.get)

  def modifyTree[S, N](f: Tree[N] => Tree[N]): NodeS[S, N, Unit] =
    RWS.modify(NodeS.treeLens.modify(f))

  def el[S]: NodeS[S, Lm[S], El] =
    RWS.inspect(NodeS.nodeLens.get)

  def ask[S, N]: NodeS[S, N, Env] =
    RWS.ask

  def tail[S, N]: NodeS[S, N, List[Tree[N]]] =
    inspectTreeF(_.tail)
}
