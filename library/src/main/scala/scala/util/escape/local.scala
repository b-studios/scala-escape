package scala.util.escape

import scala.annotation.{ Annotation, StaticAnnotation, TypeConstraint }

sealed trait Box[+T, -E] {
  def extract: T
}
object Box {
  // box introduction
  // here we need to enforce that `value` can be typed in `E`
  //def create[T, E](@local[E] value: T): Box[T, E] = new Box[T, E] { def extract = value }
  def create[T](@local[Nothing] value: T): Box[T, Nothing] = new Box[T, Nothing] { def extract = value }

  // box elimination
  def let[T, R](b: Box[T, Nothing])(f: T -> R): R = f(b.extract)
}

case object %


/*
Any = Impure
Nothing = Safe
*/

class local[-T] extends StaticAnnotation with TypeConstraint

trait `->`[-A,+B] extends Function1[A,B] {
  def apply(@local[Nothing] y: A): B
}

// Note: must be top-level, so compiler phase can detect it by prefix "->"
trait `->*`[P, -A,+B] extends Function1[A,B] {
  def apply(@local[P] y: A): B
}

object ESC {
    def NO[T](x:T):T = x
}
