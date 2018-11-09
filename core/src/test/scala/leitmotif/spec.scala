package leitmotif

import scala.concurrent.ExecutionContext

import utest.TestSuite
import xpct.XpctSpec
import cats.effect.{ContextShift, IO}
import cats.effect.internals.IOContextShift

trait Spec
extends TestSuite
with XpctSpec
{
  implicit def ec: ExecutionContext =
    scala.concurrent.ExecutionContext.Implicits.global

  implicit def ContextShift_IO: ContextShift[IO] =
    IOContextShift.global
}
