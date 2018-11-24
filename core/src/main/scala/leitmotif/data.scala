package leitmotif

case class Style()

case class Attrs(attrs: Map[String, String])

object Attrs
{
  def empty: Attrs =
    Attrs(Map())
}

sealed trait ElMeta

object ElMeta
{
  case class Regular()
  extends ElMeta

  case class Pseudo()
  extends ElMeta
}

case class El(name: String, style: Style, attrs: Attrs, meta: ElMeta)
{
  def attr(name: String): Option[String] =
    attrs.attrs.get(name)
}

object El
{
  def tag(name: String): El =
    El(name, Style(), Attrs.empty, ElMeta.Regular())
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

case class Lm[S](node: El, pre: List[LmS[S, Unit]], post: List[LmS[S, Unit]], trans: List[Trans[S]])
{
  def path(f: LmS[S, Unit]): Lm[S] =
    copy(trans = Trans.Path[S](f) :: trans)

  def sub(f: LmS[S, Unit]): Lm[S] =
    copy(trans = trans :+ Trans.Sub[S](f))
}

object Lm
{
  def default[S](node: El): Lm[S] =
    Lm(node, Nil, Nil, List(Trans.Rec(), Trans.PostRec()))

  def plain[S](tag: String): Lm[S] =
    default(El.tag(tag))
}
