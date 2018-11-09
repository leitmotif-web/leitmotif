package leitmotif

import cats.Eval
import cats.free.Cofree
import cats.implicits._

object Compile
{
  def apply[A, B](context: Context[A, B]): Tree[Lm[A, B]] => Eval[(Context[A, B], Tree[Lm[A, B]])] = {
    case Cofree(head, tail) =>
      val (path, node) = head.pathTrans(context.path, head.node)
      val context1 = context.copy(path = path)
      for {
        sub <- tail
        (context2, updated) <- sub.foldM[Eval, (Context[A, B], List[Tree[Lm[A, B]]])]((context1, Nil)) {
          case ((c, s0), a) => apply(c)(a).map {
            case (c1, s1) => (c1, s0 :+ s1)
          }
        }
      } yield (context2, Cofree(Lm[A, B](node), Eval.now(updated)))
  }
}
