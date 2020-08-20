package scala.util
package object escape {

  import scala.annotation.{Annotation, StaticAnnotation, TypeConstraint}

  class captures[T] extends StaticAnnotation with TypeConstraint

  type pure = captures[Nothing]
  type Pure[T] = Box[T, Nothing]

  sealed trait Box[+T, +E] {
    def extract: T
    def let[R](f: T -> R): R = f(extract)
  }

  def pure[T](@captures[Nothing] value: T): Box[T, Nothing] = new Box[T, Nothing] {
    def extract = value
  }

  trait `->`[-A, +B] extends Function1[A, B] {
    def apply(@captures[Nothing] y: A): B
  }

  // Note: must be top-level, so compiler phase can detect it by prefix "->"
  trait `->*`[P, -A, +B] extends Function1[A, B] {
    def apply(@captures[P] y: A): B
  }

  object ESC {
    def NO[T](x: T): T = x
  }

}