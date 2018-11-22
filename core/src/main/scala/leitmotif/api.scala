package leitmotif

import cats.Eval
import cats.data.RWS
import cats.free.Cofree
import cats.implicits._

object Render
{
  def apply[S](tree: Tree[Lm[S]]): Tree[El] =
    tree.map(_.node)
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
