package leitmotif

import cats.free.Cofree

object `package`
{
  type Tree[A] = Cofree[List, A]
}
