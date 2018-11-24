package leitmotif

case class Style()

case class Attrs(attrs: Map[String, String])

sealed trait El

object El
{
  case class Regular(name: String, style: Style, attrs: Attrs)
  extends El

  case class Pseudo()
  extends El

  def apply(name: String, style: Style, attrs: Attrs): El = Regular(name, style, attrs)
}

case class EnvPre()

case class PathEnv()

case class SubEnv(count: Int)

case class Env(path: PathEnv, sub: SubEnv)

sealed trait Trans[S]

object Trans
{
  case class Rec[S]()
  extends Trans[S]

  case class PostRec[S]()
  extends Trans[S]

  case class Path[S](f: LmS[S, Unit])
  extends Trans[S]

  case class Sub[S](f: LmS[S, Unit])
  extends Trans[S]
}

case class Lm[S](node: El, trans: List[Trans[S]])
{
  def path(f: LmS[S, Unit]): Lm[S] =
    copy(trans = Trans.Path[S](f) :: trans)

  def sub(f: LmS[S, Unit]): Lm[S] =
    copy(trans = trans :+ Trans.Sub[S](f))
}

object Lm
{
  def default[S](node: El): Lm[S] =
    Lm(node, List(Trans.Rec(), Trans.PostRec()))

  def plain[S](tag: String): Lm[S] =
    default(El(tag, Style(), Attrs(Map())))
}
