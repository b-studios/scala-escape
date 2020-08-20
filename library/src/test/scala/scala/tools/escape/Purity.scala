package scala.tools.escape

import scala.util.escape._

import org.junit.Test
import org.junit.Assert.assertEquals

class PurityTestSuite extends CompilerTesting {

  def test = {

    sealed trait A
    sealed trait B
    sealed trait C

    def test(@captures[A with B] x: Int, @captures[A] y: Int): Box[Int, A with B with C] = box {
      x + 1 + y
    }

    val b: Box[Int, Empty] = ???

    val b2: Box[Int, Nothing] = b

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
    @pure def writer(@pure x: Int) = z = x

    // actually, this is fine since both writer, as well as its argument are declared pure
    @pure val storingResult = writer(3)

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

    def parametric[E](x: Box[() => Int, E]) = x let { f =>
      f()
    }

    class Exc {
      def raise() = sys error "Fail!"
    }

    def handle[R](@pure prog: Exc => R)(@pure handler: () => R): R =
     try { prog(new Exc()) } catch {
       case e => handler()
     }

    // with the above signature, we can of course leak the exc capability
    (handle { exc => exc } { () => ??? }).raise()

    // if we want to express that exc cannot be part of the return type, R has to be wrapped in Pure
    def handle2[R](@pure prog: Exc => Pure[R])(@pure handler: () => Pure[R]): Pure[R] =
      handle(prog)(handler)

    // now this will not type check anymore:
    //    (handle2 { exc => pure { exc } } { () => ??? }) let { exc =>
    //      exc.raise()
    //    }

    // typically, a handler would not care if a handled program closes over other capabilities and we
    // would thus write:
    def handle3[R](prog: Exc => Pure[R])(handler: () => R): R =
      try { prog(new Exc()).extract } catch {
        case e => handler()
      }

    // of course this seems very restrictive since the result cannot close over *anything*
    handle3 { exc =>
      if (Math.random() > 0.5)
        exc.raise()
      else
        pure { () }
    } {
      () => ()
    }

    // but remember, that the result can close over pure values
    handle3 { exc =>
      @pure val rand = Math.random()
      if (rand > 0.5)
        exc.raise()
      else
        pure { rand }
    } {
      () => 0.0
    }
  }


  val mapPure =
    """
      | def mapPure[A, B](l: List[Pure[A]])(f: Pure[Pure[A] => B]): List[B] = l match {
      |   case Nil => Nil
      |   case (head :: tail) =>
      |     f let { fsafe =>
      |       fsafe(head) :: mapPure(tail)(f)
      |   }
      | }
      |""".stripMargin

  @Test def cannotReferenceImpureListInMap = expectEscErrorOutput(
    "value l cannot be used here. The current scope is expected to only capture: util.escape.Empty",
    mapPure ++ """
    val l: List[Pure[Int]] = List(pure(1), pure(2), pure(3))

    mapPure(l)(pure[Pure[Int] => Int] { a =>
      l
      a let { _ + 1 }
    })
    """)

  @Test def cannotPassCapAsPureParameter = expectEscErrorOutput(
    "value cap cannot be used here. It is expected to capture: util.escape.Empty",
    """
    val cap = 0
    def foo(@pure x: Int): Int = x
    foo(cap)
    """)

  @Test def cannotStoreCapInPureVariable = expectEscErrorOutput(
    "value cap cannot be used here. It is expected to capture: util.escape.Empty",
    """
    val cap = 0
    @pure var x = 1
    x = cap
    """)
}