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

case class Lm[E, S](node: El, preTrans: List[NodeS[E, S, Lm[E, S], Unit]], postTrans: List[NodeS[E, S, Lm[E, S], Unit]])
{
  def pre(f: NodeS[E, S, Lm[E, S], Unit]): Lm[E, S] =
    copy(preTrans = f :: preTrans)

  def post(f: NodeS[E, S, Lm[E, S], Unit]): Lm[E, S] =
    copy(postTrans = f :: postTrans)

  def style(styles: (String, String)*): Lm[E, S] =
    copy(node = node.copy(style = node.style.copy(static = node.style.static ++ styles)))
}

object Lm
extends LmInstances
{
  def default[E, S](node: El): Lm[E, S] =
    Lm(node, Nil, Nil)

  def plain[E, S](tag: String): Lm[E, S] =
    default(El.tag(tag))

  def withClass[E, S](tag: String, cls: String): Lm[E, S] =
    default(El.tag(tag).copy(attrs = Attrs(Map("class" -> cls))))

  def nodeLens[E, S]: Lens[Lm[E, S], El] = GenLens[Lm[E, S]](_.node)
}

trait LmInstances
{
  implicit def Transformations_Lm[E, S]: NodeTransformations[E, S, Lm[E, S]] =
    new NodeTransformations[E, S, Lm[E, S]] {
      def pre(a: Lm[E, S]): List[NodeS[E, S, Lm[E, S], Unit]] =
        a.preTrans

      def post(a: Lm[E, S]): List[NodeS[E, S, Lm[E, S], Unit]] =
        a.postTrans
    }
}
