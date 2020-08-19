package scala.tools.escape

import scala.tools.nsc.{ Global, Mode }
import scala.tools.nsc.MissingRequirementError

import scala.language.postfixOps

abstract class EscAnnotationChecker extends EscUtils {
  val global: Global
  import global._
  import analyzer.{AnalyzerPlugin, Typer}
  import definitions._

  /**
   *  Checks whether @safe annotations conform
   *
   *  NOT USED ANYMORE
   */
  object checker extends AnnotationChecker {

    /** Check annotations to decide whether tpe1 <:< tpe2 */
    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = true

    /** Refine the bounds on type parameters to the given type arguments. */
    override def adaptBoundsToAnnotations(bounds: List[TypeBounds], tparams: List[Symbol], targs: List[Type]): List[TypeBounds] =
      bounds
  }

  object plugin extends AnalyzerPlugin {

    override def canAdaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Boolean = {
      false
    }

    override def adaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Tree = {
      tree
    }

    override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type =
      tpe
  }
}
