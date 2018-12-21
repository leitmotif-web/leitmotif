package leitmotif

import cats.Eval
import monocle.Lens
import monocle.macros.GenLens
import monocle.macros.syntax.lens._

case class Style(static: Map[String, String])

object Style
{
  def empty: Style =
    Style(Map.empty)
}

case class Attrs(attrs: Map[String, String])

object Attrs
{
  def empty: Attrs =
    Attrs(Map.empty)
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
    El(name, Style.empty, Attrs.empty, ElMeta.Regular())
}

case class EnvPre()

case class PathEnv()

case class SubEnv(count: Int)

case class Env(path: PathEnv, sub: SubEnv)

object Env
{
  def recordSubCount[N]: (Env, Tree[N]) => Eval[Env] =
    (env, tree) =>
      for {
        tail <- tree.tail
      } yield env.lens(_.sub.count).modify(_ + tail.length)

  def defaultTrans[N]: EnvTransformations[Env] =
    new EnvTransformations[Env] {
      def pre[N]: List[(Env, Tree[N]) => Eval[Env]] =
        List()

      def post[N]: List[(Env, Tree[N]) => Eval[Env]] =
        List(recordSubCount[N])
    }
}

case class Lm[E, S, L](
  node: El,
  preTrans: List[NodeS[E, S, L, Lm[E, S, L], Unit]],
  postTrans: List[NodeS[E, S, L, Lm[E, S, L], Unit]],
)
{
  def pre(f: NodeS[E, S, L, Lm[E, S, L], Unit]): Lm[E, S, L] =
    copy(preTrans = f :: preTrans)

  def post(f: NodeS[E, S, L, Lm[E, S, L], Unit]): Lm[E, S, L] =
    copy(postTrans = f :: postTrans)

  def style(styles: (String, String)*): Lm[E, S, L] =
    copy(node = node.copy(style = node.style.copy(static = node.style.static ++ styles)))
}

object Lm
extends LmInstances
{
  def default[E, S, L](node: El): Lm[E, S, L] =
    Lm(node, Nil, Nil)

  def plain[E, S, L](tag: String): Lm[E, S, L] =
    default(El.tag(tag))

  def withClass[E, S, L](tag: String, cls: String): Lm[E, S, L] =
    default(El.tag(tag).copy(attrs = Attrs(Map("class" -> cls))))

  def nodeLens[E, S, L]: Lens[Lm[E, S, L], El] = GenLens[Lm[E, S, L]](_.node)
}

trait LmInstances
{
  implicit def Transformations_Lm[E, S, L]: NodeTransformations[E, S, L, Lm[E, S, L]] =
    new NodeTransformations[E, S, L, Lm[E, S, L]] {
      def pre(a: Lm[E, S, L]): List[NodeS[E, S, L, Lm[E, S, L], Unit]] =
        a.preTrans

      def post(a: Lm[E, S, L]): List[NodeS[E, S, L, Lm[E, S, L], Unit]] =
        a.postTrans
    }
}
