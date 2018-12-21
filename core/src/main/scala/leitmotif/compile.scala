package leitmotif

import cats.Eval
import cats.data.StateT
import cats.free.Cofree
import cats.implicits._

import CompileS.CompileS

trait NodeTransformations[E, S, L, N]
{
  def pre(a: N): List[NodeS[E, S, L, N, Unit]]
  def post(a: N): List[NodeS[E, S, L, N, Unit]]
}

trait EnvTransformations[E]
{
  def pre[N]: List[(E, Tree[N]) => Eval[E]]
  def post[N]: List[(E, Tree[N]) => Eval[E]]
}

object Compile
{
  def consumer[E, S, L, N](trans: List[NodeS[E, S, L, N, Unit]])(tree0: Tree[N]): CompileS[E, S, L, Tree[N]] =
    StateT {
      case Compilation(state0, env, log0) =>
        for {
          (log1, NodeState(state1, tree1), out) <- trans.sequence.run(env, NodeState(state0, tree0))
        } yield (Compilation(state1, env, log0 ++ log1), tree1)
    }

  def transEnv[E, S, L, N](trans: List[(E, Tree[N]) => Eval[E]])(tree: Tree[N]): CompileS[E, S, L, Unit] =
    CompileS.modifyEnvF(env => trans.foldM(env)((z, a) => a(z, tree)))

  def run[E, S, L, N](
    nodeTrans: NodeTransformations[E, S, L, N],
    envTrans: EnvTransformations[E],
  ): Tree[N] => CompileS[E, S, L, Tree[N]] = {
    case tree @ Cofree(head, _) =>
      for {
        tree1 <- consumer(nodeTrans.pre(head))(tree)
        _ <- transEnv(envTrans.pre[N])(tree1)
        tail0 <- CompileS.liftF(tree1.tail)
        tail1 <- tail0.traverse(run(nodeTrans, envTrans))
        tree2 = Tree.mount(tail1)(tree1)
        _ <- transEnv(envTrans.post[N])(tree2)
        tree3 <- consumer(nodeTrans.post(head))(tree2)
      } yield tree3
  }

  def apply[E, S, L, N]
  (env: E, s: S)
  (tree: Tree[N])
  (envTrans: EnvTransformations[E])
  (implicit nodeTrans: NodeTransformations[E, S, L, N])
  : Eval[(S, Tree[N], Vector[L])] =
    for {
      (Compilation(s1, _, log), tree1) <- run(nodeTrans, envTrans)(tree).run(Compilation(s, env, Vector.empty))
    } yield (s1, tree1, log)

  def default[S, L, N]
  (env: Env, s: S)
  (tree: Tree[N])
  (implicit nodeTrans: NodeTransformations[Env, S, L, N])
  : Eval[(S, Tree[N], Vector[L])] =
    apply(env, s)(tree)(Env.defaultTrans)
}
