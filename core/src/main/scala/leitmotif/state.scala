package leitmotif

import cats.Eval
import cats.data.{RWS, State, StateT}
import monocle.Lens
import monocle.macros.GenLens

case class LmState[S](s: S, tree: Tree[Lm[S]])

object LmState
{
  type LmS[S, A] = RWS[Env, Vector[String], LmState[S], A]

  def treeLens[S]: Lens[LmState[S], Tree[Lm[S]]] = GenLens[LmState[S]](_.tree)

  def sLens[S]: Lens[LmState[S], S] = GenLens[LmState[S]](_.s)

  def headLens[S]: Lens[Tree[Lm[S]], Lm[S]] = GenLens[Tree[Lm[S]]](_.head)

  def nodeLens[S]: Lens[LmState[S], El] = treeLens.composeLens(headLens[S]).composeLens(Lm.nodeLens)
}

case class LmIState[S](state: S, env: Env, log: Vector[String])

object LmIState
{
  type LmIS[S, A] = State[LmIState[S], A]

  def envLens[S]: Lens[LmIState[S], Env] =
    GenLens[LmIState[S]](_.env)

  def liftF[S, A](fa: Eval[A]): LmIS[S, A] =
    StateT.liftF(fa)

  def modifyEnv[S](f: Env => Env): LmIS[S, Unit] =
    State.modify(envLens.modify(f))
}
