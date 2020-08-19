package scala.tools.escape

import scala.util.escape._

import org.junit.Test
import org.junit.Assert.assertEquals

class PurityTestSuite extends CompilerTesting {


  def test = {

    // Creating a Box[T, Nothing] requires all bindings in the environment to be binding <:< @local[Nothing]

    val x = pure { 4 }

    val cap = 42

    def foo(@pure x: Int): Int = x

    foo(2)

    def bar(@pure y: Int) = pure {
      1 + y
    }

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
      //l
      a let { _ + 1 }
    })
  }
}