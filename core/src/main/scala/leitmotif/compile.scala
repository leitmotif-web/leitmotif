package leitmotif

import cats.Eval
import cats.data.StateT
import cats.free.Cofree
import cats.implicits._
import monocle.macros.syntax.lens._

import CompileS.CompileS

trait Transformations[S, N]
{
  def pre(a: N): List[NodeS[S, N, Unit]]
  def post(a: N): List[NodeS[S, N, Unit]]
}

object Compile
{
  def consumer[S, N](trans: List[NodeS[S, N, Unit]])(tree0: Tree[N]): CompileS[S, Tree[N]] =
    StateT {
      case Compilation(state0, env, log0) =>
        for {
          (log1, NodeState(state1, tree1), out) <- trans.sequence.run(env, NodeState(state0, tree0))
        } yield (Compilation(state1, env, log0 ++ log1), tree1)
    }

  def run[S, N](trans: Transformations[S, N]): Tree[N] => CompileS[S, Tree[N]] = {
    case tree @ Cofree(head, _) =>
      for {
        tree1 <- consumer(trans.pre(head))(tree)
        tail0 <- CompileS.liftF(tree1.tail)
        tail1 <- tail0.traverse(run[S, N](trans))
        _ <- CompileS.modifyEnv[S](_.lens(_.sub.count).modify(_ + tail0.length))
        tree2 <- consumer(trans.post(head))(Tree.mount(tail1)(tree1))
      } yield tree2
  }

  def apply[S, N]
  (env: Env, s: S)
  (tree: Tree[N])
  (implicit trans: Transformations[S, N])
  : Eval[(S, Tree[N])] = {
    for {
      (Compilation(s1, _, _), tree1) <- run(trans)(tree).run(Compilation(s, env, Vector.empty))
    } yield (s1, tree1)
  }
}
