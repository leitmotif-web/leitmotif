package leitmotif

import cats.Eval
import cats.data.{RWS, State, StateT}
import monocle.Lens
import monocle.macros.GenLens

case class NodeState[S, N](s: S, tree: Tree[N])

object NodeS
{
  type NodeS[E, S, N, A] = RWS[E, Vector[String], NodeState[S, N], A]

  def treeLens[E, S, N]: Lens[NodeState[S, N], Tree[N]] = GenLens[NodeState[S, N]](_.tree)

  def sLens[E, S, N]: Lens[NodeState[S, N], S] = GenLens[NodeState[S, N]](_.s)

  def headLens[E, S, N]: Lens[Tree[Lm[E, S]], Lm[E, S]] = GenLens[Tree[Lm[E, S]]](_.head)

  def nodeLens[E, S]: Lens[NodeState[S, Lm[E, S]], El] =
    treeLens
      .composeLens(headLens[E, S, Lm[E, S]])
      .composeLens(Lm.nodeLens)
}

case class Compilation[E, S](state: S, env: E, log: Vector[String])

object CompileS
{
  type CompileS[E, S, A] = State[Compilation[E, S], A]

  def envLens[E, S]: Lens[Compilation[E, S], E] =
    GenLens[Compilation[E, S]](_.env)

  def liftF[E, S, A](fa: Eval[A]): CompileS[E, S, A] =
    StateT.liftF(fa)

  def modifyEnv[E, S](f: E => E): CompileS[E, S, Unit] =
    State.modify(envLens.modify(f))

  def modifyEnvF[E, S](f: E => Eval[E]): CompileS[E, S, Unit] =
    StateT.modifyF { s =>
      for {
        e <- f(s.env)
      } yield s.copy(env = e)
    }
}
