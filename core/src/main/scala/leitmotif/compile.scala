package leitmotif

import cats.Eval
import cats.free.Cofree
import cats.implicits._

object Compile
{
  def trans[A, B]
  (tail: Eval[List[Tree[Lm[A, B]]]])
  (z: (Context[A, B], Tree[Lm[A, B]]), a: Trans[A, B])
  : Eval[(Context[A, B], Tree[Lm[A, B]])] = {
    val (context, tree) = z
    a match {
      case Trans.Rec() =>
        for {
          sub <- tail
          (updatedContext, updatedSub) <- sub.foldM[Eval, (Context[A, B], List[Tree[Lm[A, B]]])]((context, Nil)) {
            case ((c, s0), a) => apply(c)(a).map {
              case (c1, s1) => (c1, s0 :+ s1)
            }
          }
        } yield (updatedContext, Cofree(tree.head, Eval.now(updatedSub)))
      case Trans.Path(f) =>
        val (path, node) = f(context.path, tree.head.node)
        val updatedContext = context.copy(path = path)
        val updatedTree = tree.copy(head = tree.head.copy(node = node))
        Eval.now((updatedContext, updatedTree))
      case _ =>
        ???
    }
  }

  def apply[A, B](context: Context[A, B]): Tree[Lm[A, B]] => Eval[(Context[A, B], Tree[Lm[A, B]])] = {
    case tree @ Cofree(head, tail) =>
      head.trans.foldM((context, tree))(trans(tail))
  }
}
