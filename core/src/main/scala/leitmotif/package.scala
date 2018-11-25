package leitmotif

object `package`
{
  type Tree[A] = Tree.Tree[A]
  type LmS[S, A] = LmState.LmS[S, A]
  val LmS = LmState
  val LmIS = LmIState
}
