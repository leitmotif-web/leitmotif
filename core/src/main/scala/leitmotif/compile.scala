package leitmotif

import cats.Eval
import cats.data.StateT
import cats.free.Cofree
import cats.implicits._
import monocle.macros.syntax.lens._

import LmIState.LmIS

object Compile
{
  def consumer[S](trans: List[LmS[S, Unit]]): LmIS[S, Unit] =
    StateT {
      case LmIState(state0, env, log0) =>
        for {
          (log1, state1, a) <- trans.sequence.run(env, state0)
        } yield (LmIState(state1, env, log0 ++ log1), ())
    }

  def run[S]: Tree[Lm[S]] => LmIS[S, Tree[Lm[S]]] = {
    case tree @ Cofree(head, tail) =>
      for {
        _ <- LmIS.setTree(tree)
        _ <- consumer(head.preTrans)
        tree1 <- LmIS.tree
        tail0 <- LmIS.liftF(tail)
        tail1 <- tail0.traverse(run)
        _ <- LmIS.setTree(tree1)
        _ <- LmIS.modifyEnv(_.lens(_.sub.count).modify(_ + tail0.length))
        _ <- LmIS.modifyTree[S](_.copy(tail = Eval.now(tail1)))
        _ <- consumer(head.postTrans)
        tree2 <- LmIS.tree
      } yield tree2
  }

  def apply[S](env: Env, s: S)(tree: Tree[Lm[S]]): Eval[(S, Tree[Lm[S]])] = {
    for {
      (LmIState(LmState(s1, tree1), _, _), _) <- run(tree).run(LmIState(LmState(s, tree), env, Vector.empty))
    } yield (s1, tree1)
  }
}
