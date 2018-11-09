package leitmotif

import cats.Eval
import cats.free.Cofree
import cats.implicits._

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

case class ContextPre()

case class Context[A, B](path: A, sub: B)

case class Lm[A, B](node: El, pathTrans: (A, El) => (A, El), subTrans: (B, El) => (B, El))
{
  def path(f: (A, El) => (A, El)): Lm[A, B] =
    copy(pathTrans = f)

  def sub(f: (B, El) => (B, El)): Lm[A, B] =
    copy(subTrans = f)
}

object Lm
{
  def apply[A, B](node: El): Lm[A, B] =
    Lm(node, (a, e) => (a, e), (b, e) => (b, e))
}

object Compile
{
  def apply[A, B](context: Context[A, B]): Tree[Lm[A, B]] => Eval[(Context[A, B], Tree[Lm[A, B]])] = {
    case Cofree(head, tail) =>
      val (path, node) = head.pathTrans(context.path, head.node)
      val context1 = context.copy(path = path)
      for {
        sub <- tail
        (context2, updated) <- sub.foldM[Eval, (Context[A, B], List[Tree[Lm[A, B]]])]((context1, Nil)) {
          case ((c, s0), a) => apply(c)(a).map {
            case (c1, s1) => (c1, s0 :+ s1)
          }
        }
      } yield (context2, Cofree(Lm[A, B](node), Eval.now(updated)))
  }
}

object Render
{
  def apply[A, B](tree: Tree[Lm[A, B]]): Tree[El] =
    tree.map(_.node)
}

object Leitmotif
{
  def node[A, B](head: Lm[A, B])(tail: Tree[Lm[A, B]]*): Tree[Lm[A, B]] =
    Cofree(head, Eval.now(tail.toList))
}
