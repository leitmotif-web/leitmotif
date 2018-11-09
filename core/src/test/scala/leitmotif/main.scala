package leitmotif

import utest._
import cats.free.Cofree
import cats.Eval

object LmSpec
extends MktSpec
{
  def tree = Cofree[List, LmPre](LmPre(El("section", Style(), Attrs()), (_, a) => a), Eval.now(Nil))

  def test1 = {
    val result = Leitmotif.compile(Context(0))(tree)
    println(result)
  }

  def tests = Tests("foo" - test1)
}
