package leitmotif

import cats.Eval
import cats.free.Cofree
import cats.implicits._

object Compile
{
  def trans[S]
  (tail: Eval[List[Tree[Lm[S]]]])
  (z: (Env, S, Tree[Lm[S]]), a: Trans[S])
  : Eval[(Env, S, Tree[Lm[S]])] = {
    val (env, s, tree) = z
    a match {
      case Trans.Rec() =>
        for {
          sub <- tail
          (updatedEnv, updatedS, updatedSub) <- sub.foldM[Eval, (Env, S, List[Tree[Lm[S]]])]((env, s, Nil)) {
            case ((e, s0, sub0), a) =>
              apply(e, s0)(a).map {
                case (e1, s1, sub1) => (e1, s1, sub0 :+ sub1)
              }
          }
        } yield (updatedEnv, updatedS, Cofree(tree.head, Eval.now(updatedSub)))
      case Trans.Path(f) =>
        val (_, (s1, node), _) = f.run(env, (s, tree.head.node)).value
        val updatedTree = tree.copy(head = tree.head.copy(node = node))
        Eval.now((env, s1, updatedTree))
      case Trans.Sub(f) =>
        val (_, (s1, node), _) = f.run(env, (s, tree.head.node)).value
        val updatedTree = tree.copy(head = tree.head.copy(node = node))
        Eval.now((env, s1, updatedTree))
    }
  }

  def apply[S](env: Env, s: S): Tree[Lm[S]] => Eval[(Env, S, Tree[Lm[S]])] = {
    case tree @ Cofree(head, tail) =>
      head.trans.foldM((env, s, tree))(trans(tail))
  }
}
