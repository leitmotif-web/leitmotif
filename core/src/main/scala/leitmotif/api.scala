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

  def inspectS[S, A](f: S => A): LmS[S, A] =
    RWS.inspect(f compose LmState.sLens.get)

  def modifyS[S](f: S => S): LmS[S, Unit] =
    RWS.modify(LmState.sLens.modify(f))

  def modifyEl[S](f: El => El): LmS[S, Unit] =
    RWS.modify(LmState.nodeLens.modify(f))

  def inspectTreeF[S, A](f: Tree[Lm[S]] => Eval[A]): LmS[S, A] =
    IRWST.inspectF(f compose LmState.treeLens.get)

  def modifyTree[S](f: Tree[Lm[S]] => Tree[Lm[S]]): LmS[S, Unit] =
    RWS.modify(LmState.treeLens.modify(f))

  def el[S]: LmS[S, El] =
    RWS.inspect(LmState.nodeLens.get)

  def ask[S]: LmS[S, Env] =
    RWS.ask

  def tail[S]: LmS[S, List[Tree[Lm[S]]]] =
    inspectTreeF(_.tail)
}
