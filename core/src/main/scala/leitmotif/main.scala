package leitmotif

import cats.Eval
import cats.free.Cofree
import cats.implicits._
import scalatags.Text.Tag

case class Style()

case class Attrs()

sealed trait El

object El
{
  case class Regular(name: String, style: Style, attrs: Attrs)
  extends El

  case class Pseudo()
  extends El

  def apply(name: String, style: Style, attrs: Attrs): El = Regular(name, style, attrs)
}

case class Context(headline: Int)

case class LmPre(node: El, pre: (Context, El) => El)

case class Lm(node: El)

object Leitmotif
{
  import scalatags.Text.all._
  type Tree[A] = Cofree[List, A]

  def compile(context: Context): Tree[LmPre] => Tree[Lm] = {
    case Cofree(head, tail) =>
      Cofree(Lm(head.node), tail.map(_.map(compile(context))))
  }

  def render(tree: Tree[Lm]): Eval[Tag] =
    Cofree.cata[List, Lm, Tag](tree) {
      ???
    }
}
