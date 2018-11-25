package leitmotif

import cats.Eval
import cats.data.{RWS, State, StateT}
import monocle.Lens
import monocle.macros.GenLens

case class NodeState[S](s: S, tree: Tree[Lm[S]])

object NodeState
{
  type NodeS[S, A] = RWS[Env, Vector[String], NodeState[S], A]

  def treeLens[S]: Lens[NodeState[S], Tree[Lm[S]]] = GenLens[NodeState[S]](_.tree)

  def sLens[S]: Lens[NodeState[S], S] = GenLens[NodeState[S]](_.s)

  def headLens[S]: Lens[Tree[Lm[S]], Lm[S]] = GenLens[Tree[Lm[S]]](_.head)

  def nodeLens[S]: Lens[NodeState[S], El] = treeLens.composeLens(headLens[S]).composeLens(Lm.nodeLens)
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
