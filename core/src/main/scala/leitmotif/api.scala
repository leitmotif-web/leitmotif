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

  def inspectS[S, A](f: S => A): NodeS[S, A] =
    RWS.inspect(f compose NodeState.sLens.get)

  def modifyS[S](f: S => S): NodeS[S, Unit] =
    RWS.modify(NodeState.sLens.modify(f))

  def modifyEl[S](f: El => El): NodeS[S, Unit] =
    RWS.modify(NodeState.nodeLens.modify(f))

  def inspectTreeF[S, A](f: Tree[Lm[S]] => Eval[A]): NodeS[S, A] =
    IRWST.inspectF(f compose NodeState.treeLens.get)

  def modifyTree[S](f: Tree[Lm[S]] => Tree[Lm[S]]): NodeS[S, Unit] =
    RWS.modify(NodeState.treeLens.modify(f))

  def el[S]: NodeS[S, El] =
    RWS.inspect(NodeState.nodeLens.get)

  def ask[S]: NodeS[S, Env] =
    RWS.ask

  def tail[S]: NodeS[S, List[Tree[Lm[S]]]] =
    inspectTreeF(_.tail)
}
