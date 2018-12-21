package leitmotif

import cats.Eval
import cats.data.{RWS, State, StateT}
import monocle.Lens
import monocle.macros.GenLens

case class NodeState[S, N](s: S, tree: Tree[N])

object NodeS
{
  type NodeS[E, S, L, N, A] = RWS[E, Vector[L], NodeState[S, N], A]

  def treeLens[E, S, L, N]: Lens[NodeState[S, N], Tree[N]] = GenLens[NodeState[S, N]](_.tree)

  def sLens[E, S, L, N]: Lens[NodeState[S, N], S] = GenLens[NodeState[S, N]](_.s)

  def headLens[E, S, L, N]: Lens[Tree[Lm[E, S, L]], Lm[E, S, L]] = GenLens[Tree[Lm[E, S, L]]](_.head)

  def nodeLens[E, S, L]: Lens[NodeState[S, Lm[E, S, L]], El] =
    treeLens
      .composeLens(headLens[E, S, L, Lm[E, S, L]])
      .composeLens(Lm.nodeLens)
}

case class Compilation[E, S, L](state: S, env: E, log: Vector[L])

object CompileS
{
  type CompileS[E, S, L, A] = State[Compilation[E, S, L], A]

  def envLens[E, S, L]: Lens[Compilation[E, S, L], E] =
    GenLens[Compilation[E, S, L]](_.env)

  def liftF[E, S, L, A](fa: Eval[A]): CompileS[E, S, L, A] =
    StateT.liftF(fa)

  def modifyEnv[E, S, L](f: E => E): CompileS[E, S, L, Unit] =
    State.modify(envLens.modify(f))

  def modifyEnvF[E, S, L](f: E => Eval[E]): CompileS[E, S, L, Unit] =
    StateT.modifyF { s =>
      for {
        e <- f(s.env)
      } yield s.copy(env = e)
    }
}
