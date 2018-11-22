package leitmotif

import cats.free.Cofree
import cats.data.ReaderWriterState

object `package`
{
  type Tree[A] = Cofree[List, A]
  // type LmS[A] = PartialFunction[(A, El), (A, El)]
  type LmS[S, A] = ReaderWriterState[Env, List[String], (S, El), A]
}
