package leitmotif

import cats.Eval
import cats.free.Cofree
import cats.implicits._

object Render
{
  def apply[A, B](tree: Tree[Lm[A, B]]): Tree[El] =
    tree.map(_.node)
}

object Leitmotif
{
  def node[A, B](head: Lm[A, B])(tail: Tree[Lm[A, B]]*): Tree[Lm[A, B]] =
    Cofree(head, Eval.now(tail.toList))
}
