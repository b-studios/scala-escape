package scala.tools.escape

import scala.util.escape._

import org.junit.Test
import org.junit.Assert.assertEquals

class PurityTestSuite extends CompilerTesting {


  def test = {

    // Creating a Box[T, Nothing] requires all free variables to be <:< @captures[Nothing]
    val x = pure { 4 }

    val cap = 42

    def foo(@pure x: Int): Int = x

    // in the paper the above is not possible, there you would need to write

    def foo2(x: Pure[Int]): Int = x let { v => v }

    foo(2)
    foo2(pure { 2 })

    // what is also not possible in the paper is to annotate val bindings:
    @pure val n = 4
    foo(n)

    // mutable variables can also be declared as @pure
    @pure var z = 4

    def bar(@pure y: Int) = pure {
      1 + z
    }

    // but that means they cannot close over things:
    // @pure var z2 = cap // value cap cannot be used here.

    // we can store pure values in pure variables:
    def writer(@pure x: Int) = z = x

    // impure things can be stored in impure variables
    var state = 0
    state = cap

    // trying the same with something impure does not work:
//     def writer2(x: Int) = z = x

    def baz(b: Pure[Int]) = {
      b let { x =>
        pure { x }
      }
    }

    def mapPure[A, B](l: List[Pure[A]])(f: Pure[Pure[A] => B]): List[B] = l match {
      case Nil => Nil
      case (head :: tail) =>
        f let { fsafe =>
          fsafe(head) :: mapPure(tail)(f)
        }
    }

    val l: List[Pure[Int]] = List(pure(1), pure(2), pure(3))

    mapPure(l)(pure[Pure[Int] => Int] { a =>
      // referencing l here is not allowed:
      // l
      a let { _ + 1 }
    })
  }
}