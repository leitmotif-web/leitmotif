package leitmotif

import cats.Eval

case class StyleRule(key: String, value: String)

case class StyleBlock(rules: List[StyleRule])

case class Stylesheet(blocks: List[StyleBlock])

object AggregateStyle
{
  def trans[S, L, N]
  (basic: NodeTransformations[Env, S, L, N])
  (gen: NodeS[Env, S, L, N, Unit])
  : NodeTransformations[Env, S, L, N] =
    new NodeTransformations[Env, S, L, N] {
      def pre(a: N): List[NodeS[Env, S, L, N, Unit]] = basic.pre(a)
      def post(a: N): List[NodeS[Env, S, L, N, Unit]] = basic.post(a) :+ gen
    }

  def apply[S, L, N]
  (env: Env, s: S)
  (tree: Tree[N])
  (gen: NodeS[Env, S, L, N, Unit])
  (implicit nodeTrans: NodeTransformations[Env, S, L, N])
  : Eval[Vector[L]] =
    for {
      (_, _, style) <- Compile.default(env, s)(tree)(trans(nodeTrans)(gen))
    } yield style
}
