package efa.nb

import scalaz._, Scalaz._, effect._, scalacheck.ScalazArbitrary._
import org.scalacheck._, Prop._
import efa.react._

object UndoEditTest extends Properties ("UndoEdit") {
  import UndoEdit._

  property("pairs") = forAll{is: NonEmptyList[Int] ⇒ 
    val exp = (is.head, is.head) :: (is.tail zip is.list) reverse

    val res = for {
      ref ← IO newIORef List.empty[(Int,Int)]
      s   ← Signal newVar is.head
      _   ← pairs[Int] to prep(ref) runIO s
      _   ← is foldMap s.put
      res ← ref.read
    } yield (exp ≟ res) :| "Exp: %s but was: %s".format(exp, res)

    testIO(res)
  }

  /**
   * Checks that input events are propagated to the output
   */
  property("undoSST_propagate") = forAll{is: NonEmptyList[Int] ⇒ 
    val exp = is.list.reverse

    val res = for {
      ref ← IO newIORef List.empty[Int]
      s   ← Signal newVar is.head
      _   ← undoSST[Int](_ ⇒ IO.ioUnit) to prep(ref) runIO s
      _   ← is.tail foldMap s.put
      res ← ref.read
    } yield (exp ≟ res) :| "Exp: %s but was: %s".format(exp, res)

    testIO(res)
  }

  /**
   * Checks undo
   */
  property("undoSST_undo") = forAll{is: NonEmptyList[Int] ⇒ 
    val exp = is.list.reverse.tail.reverse

    val res = for {
      ref ← IO newIORef List.empty[Int]
      out ← IO newIORef List.empty[UndoEdit]
      s   ← Signal newVar is.head
      _   ← undoSST[Int](prep(out)) to prep(ref) runIO s
      _   ← is.tail foldMap s.put
      _   ← ref write Nil
      _   ← out.read >>= (_ foldMap (_.un))
      res ← ref.read
    } yield (exp ≟ res) :| "Exp: %s but was: %s".format(exp, res)

    testIO(res)
  }

  /**
   * Checks redo
   */
  property("undoSST_redo") = forAll{is: NonEmptyList[Int] ⇒ 
    val exp = is.tail

    val res = for {
      ref ← IO newIORef List.empty[Int]
      out ← IO newIORef List.empty[UndoEdit]
      s   ← Signal newVar is.head
      _   ← undoSST[Int](prep(out)) to prep(ref) runIO s
      _   ← is.tail foldMap s.put
      _   ← ref write Nil
      _   ← out.read >>= (_ foldMap (_.re))
      res ← ref.read
    } yield (exp ≟ res) :| "Exp: %s but was: %s".format(exp, res)

    testIO(res)
  }

  private def testIO (io: IO[Prop]) = io.unsafePerformIO

  private def prep[A] (r: IORef[List[A]]): Out[A] =
    a ⇒ r mod (a :: _) void
}

// vim: set ts=2 sw=2 et:
