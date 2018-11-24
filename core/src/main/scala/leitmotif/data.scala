package leitmotif

import monocle.Lens
import monocle.macros.GenLens

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

case class Lm[S](node: El, preTrans: List[LmS[S, Unit]], postTrans: List[LmS[S, Unit]])
{
  def pre(f: LmS[S, Unit]): Lm[S] =
    copy(preTrans = f :: preTrans)

  def post(f: LmS[S, Unit]): Lm[S] =
    copy(postTrans = f :: postTrans)
}

object Lm
{
  def default[S](node: El): Lm[S] =
    Lm(node, Nil, Nil)

  def plain[S](tag: String): Lm[S] =
    default(El.tag(tag))

  def nodeLens[S]: Lens[Lm[S], El] = GenLens[Lm[S]](_.node)
}
