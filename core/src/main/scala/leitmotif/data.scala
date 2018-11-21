package leitmotif

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

sealed trait Trans[A, B]

object Trans
{
  case class Rec[A, B]()
  extends Trans[A, B]

  case class Path[A, B](f: (A, El) => (A, El))
  extends Trans[A, B]

  case class Sub[A, B](f: (B, El) => (B, El))
  extends Trans[A, B]
}

case class Lm[A, B](node: El, trans: List[Trans[A, B]])
{
  def path(f: (A, El) => (A, El)): Lm[A, B] =
    copy(trans = Trans.Path[A, B](f) :: trans)

  def sub(f: (B, El) => (B, El)): Lm[A, B] =
    copy(trans = trans :+ Trans.Sub[A, B](f))
}

object Lm
{
  def apply[A, B](node: El): Lm[A, B] =
    Lm(node, List(Trans.Rec()))
}
