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
