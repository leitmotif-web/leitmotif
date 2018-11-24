package leitmotif

import cats.free.Cofree

object `package`
{
  type Tree[A] = Cofree[List, A]
  type LmS[S, A] = LmState.LmS[S, A]
  val LmS = LmState
  val LmIS = LmIState
}
