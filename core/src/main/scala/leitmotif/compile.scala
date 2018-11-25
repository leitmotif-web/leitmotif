package leitmotif

import cats.Eval
import cats.data.StateT
import cats.free.Cofree
import cats.implicits._
import monocle.macros.syntax.lens._

import CompileS.CompileS

object Compile
{
  def consumer[S](trans: List[NodeS[S, Unit]])(tree0: Tree[Lm[S]]): CompileS[S, Tree[Lm[S]]] =
    StateT {
      case Compilation(state0, env, log0) =>
        for {
          (log1, NodeState(state1, tree1), out) <- trans.sequence.run(env, NodeState(state0, tree0))
        } yield (Compilation(state1, env, log0 ++ log1), tree1)
    }

  def run[S]: Tree[Lm[S]] => CompileS[S, Tree[Lm[S]]] = {
    case tree @ Cofree(head, _) =>
      for {
        tree1 <- consumer(head.preTrans)(tree)
        tail0 <- CompileS.liftF(tree1.tail)
        tail1 <- tail0.traverse(run)
        _ <- CompileS.modifyEnv(_.lens(_.sub.count).modify(_ + tail0.length))
        tree2 <- consumer(head.postTrans)(Tree.mount(tail1)(tree1))
      } yield tree2
  }

  def apply[S](env: Env, s: S)(tree: Tree[Lm[S]]): Eval[(S, Tree[Lm[S]])] = {
    for {
      (Compilation(s1, _, _), tree1) <- run(tree).run(Compilation(s, env, Vector.empty))
    } yield (s1, tree1)
  }
}
