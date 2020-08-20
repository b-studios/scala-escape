package scala.util
package object escape {

  import scala.annotation.{Annotation, StaticAnnotation, TypeConstraint}

  class captures[T] extends StaticAnnotation with TypeConstraint

  // we invert the typing lattice to allow intersection to be used for set union. In Dotty we could also
  // use union types.
  type Empty = Any

  type pure = captures[Empty]
  type Pure[T] = Box[T, Empty]

  sealed trait Box[+T, -E] {
    def extract: T
    def let[R](f: T -> R): R = f(extract)
  }

  def pure[T](@captures[Empty] value: T): Box[T, Empty] = new Box[T, Empty] {
    def extract = value
  }

  def box[T, E](@captures[E] value: T): Box[T, E] = new Box[T, E] {
    def extract = value
  }

  trait `->`[-A, +B] extends Function1[A, B] {
    def apply(@captures[Empty] y: A): B
  }

  // Note: must be top-level, so compiler phase can detect it by prefix "->"
  trait `->*`[P, -A, +B] extends Function1[A, B] {
    def apply(@captures[P] y: A): B
  }
}