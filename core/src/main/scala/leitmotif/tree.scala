package leitmotif

import cats.Eval
import cats.free.Cofree

object Tree
{
  type Tree[A] = Cofree[List, A]

  def mount[A](tail: List[Tree[A]]): Tree[A] => Tree[A] = {
    case Cofree(head, _) => Cofree(head, Eval.now(tail))
  }
}
