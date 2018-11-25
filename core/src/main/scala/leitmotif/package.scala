package leitmotif

object `package`
{
  type Tree[A] = Tree.Tree[A]
  type NodeS[S, A] = NodeState.NodeS[S, A]
  val NodeS = NodeState
}
