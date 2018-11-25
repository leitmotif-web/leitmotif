package leitmotif

import cats.Eval
import cats.data.{RWS, State, StateT}
import monocle.Lens
import monocle.macros.GenLens

case class NodeState[S, N](s: S, tree: Tree[N])

object NodeS
{
  type NodeS[S, N, A] = RWS[Env, Vector[String], NodeState[S, N], A]

  def treeLens[S, N]: Lens[NodeState[S, N], Tree[N]] = GenLens[NodeState[S, N]](_.tree)

  def sLens[S, N]: Lens[NodeState[S, N], S] = GenLens[NodeState[S, N]](_.s)

  def headLens[S, N]: Lens[Tree[Lm[S]], Lm[S]] = GenLens[Tree[Lm[S]]](_.head)

  def nodeLens[S]: Lens[NodeState[S, Lm[S]], El] = treeLens.composeLens(headLens[S, Lm[S]]).composeLens(Lm.nodeLens)
}

case class Compilation[S](state: S, env: Env, log: Vector[String])

object CompileS
{
  type CompileS[S, A] = State[Compilation[S], A]

  def envLens[S]: Lens[Compilation[S], Env] =
    GenLens[Compilation[S]](_.env)

  def liftF[S, A](fa: Eval[A]): CompileS[S, A] =
    StateT.liftF(fa)

  def modifyEnv[S](f: Env => Env): CompileS[S, Unit] =
    State.modify(envLens.modify(f))
}
