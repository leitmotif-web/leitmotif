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

  def level[E, S, L](prefix: String)(tree: Tree[Lm[E, S, L]]): Eval[List[String]] =
    for {
      tail <- tree.tail
      sub <- tail.flatTraverse(level(s"$prefix- "))
    } yield s"$prefix${node(tree.head.node)}" :: sub
}

object Render
{
  def apply[E, S, L](tree: Tree[Lm[E, S, L]]): Tree[El] =
    tree.map(_.node)

  def text[E, S, L](tree: Tree[Lm[E, S, L]]): String =
    RenderText.level("")(tree).value.mkString("\n")
}

object Leitmotif
{
  def node[E, S, L](head: Lm[E, S, L])(tail: Tree[Lm[E, S, L]]*): Tree[Lm[E, S, L]] =
    Cofree(head, Eval.now(tail.toList))

  def inspectS[E, S, L, N, A](f: S => A): NodeS[E, S, L, N, A] =
    RWS.inspect(f compose NodeS.sLens.get)

  def modifyS[E, S, L, N](f: S => S): NodeS[E, S, L, N, Unit] =
    RWS.modify(NodeS.sLens.modify(f))

  def getEl[E, S, L]: NodeS[E, S, L, Lm[E, S, L], El] =
    RWS.inspect(NodeS.nodeLens.get)

  def modifyEl[E, S, L](f: El => El): NodeS[E, S, L, Lm[E, S, L], Unit] =
    RWS.modify(NodeS.nodeLens.modify(f))

  def inspectTreeF[E, S, L, N, A](f: Tree[N] => Eval[A]): NodeS[E, S, L, N, A] =
    IRWST.inspectF(f compose NodeS.treeLens.get)

  def modifyTree[E, S, L, N](f: Tree[N] => Tree[N]): NodeS[E, S, L, N, Unit] =
    RWS.modify(NodeS.treeLens.modify(f))

  def el[E, S, L]: NodeS[E, S, L, Lm[E, S, L], El] =
    RWS.inspect(NodeS.nodeLens.get)

  def ask[E, S, L, N]: NodeS[E, S, L, N, E] =
    RWS.ask[E, Vector[L], NodeState[S, N]]

  def tail[E, S, L, N]: NodeS[E, S, L, N, List[Tree[N]]] =
    inspectTreeF(_.tail)
}
