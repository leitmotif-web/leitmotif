package leitmotif

case class StyleRule(key: String, value: String)

case class StyleBlock(rules: List[StyleRule])

case class Stylesheet(blocks: List[StyleBlock])

object AggregateStyle
{
  def apply[E, S](tree: Tree[Lm[E, S]])(gen: Lm[E, S]): Stylesheet =
    ???
}
