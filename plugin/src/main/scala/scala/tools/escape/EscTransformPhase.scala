package scala.tools.escape

import scala.tools.nsc.transform._
import scala.tools.nsc.symtab._
import scala.tools.nsc.plugins._

import scala.language.postfixOps

/**
 * Check that `@safe` symbols do not escape their defining scope.
 */
abstract class EscTransform extends PluginComponent with Transform with
  TypingTransformers with EscUtils {
  // inherits abstract value `global` and class `Phase` from Transform

  import global._ // the global environment
  import definitions._ // standard classes and methods

  override def description = "Escape check phase"

  /** the following two members override abstract members in Transform */
  val phaseName: String = "escape"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new EscTransformer(unit)

  class EscTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    def dominates(a: Symbol, b: Symbol) =
      b.owner.owner.hasTransOwner(a.owner)

    def dominates(a: Symbol, bs: Set[Symbol]) =
      bs.forall(_.owner.owner.hasTransOwner(a.owner))

    // FIXME: proper sym lookup
    def isSndFun(tpe: Type) = tpe.toString.startsWith("scala.util.escape") || tpe.toString.contains("->")


    //def isSndSym(s: Symbol) = !isFstSym(s) //s.hasAnnotation(MarkerLocal)
    def isFstSym(s: Symbol): Boolean = Pure <:< symCapabilities(s)

    def symCapabilities(s: Symbol) =
      s.getAnnotation(MarkerLocal) match {
        case None => Impure
        case Some(s) => s.atp.typeArgs(0)
      }

    lazy val Pure = AnyTpe
    lazy val Impure = NothingTpe

    /*
    TODO:

      + check inheritance
      + abstract over mode
      - objects:
          - what is the story for @local this?
          - can one call methods on @local objects?

*/

    // m is the mode -- here read "the capabilities that are allowed to be captured"
    def traverse(tree: global.Tree, m: global.Type, boundaries: List[global.Symbol]): Unit = {
      curTree = tree

      if (isBuiltin(tree.symbol)) return

      tree match {
        case Literal(x) =>
        case Ident(x) =>

          if (!(m <:< symCapabilities(tree.symbol))) {
            reporter.error(tree.pos, tree.symbol + " cannot be used here. It is expected to capture: " + m + "")
          } else {
            for (b <- boundaries) {
              if (!tree.symbol.hasTransOwner(b)) {
                val m = symCapabilities(b)
                if (!(m <:< symCapabilities(tree.symbol)))
                  reporter.error(tree.pos, tree.symbol + s" cannot be used here. The current scope is expected to only capture: ${m}")
              }
            }
          }

        case Select(qual, name) =>
          traverse(qual, m, boundaries)

        case Apply(fun, args) =>
          traverse(fun, m, boundaries)


          // find out mode to use for each parameter (1st or 2nd)
          val modes = fun.tpe match {
            case mt@MethodType(params, restpe) =>
              fun match {
                case Apply(TypeApply(Select(qual, name), targs), _) =>
                  val tps = fun.symbol.typeParams
                  val tas = targs.map { _.tpe }
                  params.map(s => symCapabilities(s).asSeenFrom(qual.tpe, fun.symbol.owner).subst(tps, tas))
                case TypeApply(Select(qual, name), targs) =>
                  // we need to substitute
                  val tps = fun.symbol.typeParams
                  val tas = targs.map { _.tpe }
                  // println(tps.toString() + " -> " + tas)

                  params.map(s => symCapabilities(s).asSeenFrom(qual.tpe, fun.symbol.owner).subst(tps, tas) )
                case Select(qual, name) =>

                  params.map(s => symCapabilities(s).asSeenFrom(qual.tpe, fun.symbol.owner))
                case Ident(_) =>
                  params.map(s => symCapabilities(s))
                case _ =>
                  //println("---> other: "+ fun.getClass + "/"+fun +"/"+fun.symbol.owner)
                  params.map(s => symCapabilities(s))
              }
            case _ => Nil
          }
          // check argument expressions according to mode
          // for varargs, assume 2nd class (pad to args.length)
          map2(args, modes.padTo(args.length, Impure)) { (a, mode) =>
            // TODO compute the intersection of arg-mode and the current m
            if (m <:< mode) traverse(a, mode, boundaries) else traverse(a, m, boundaries)
          }

        case TypeApply(fun, args) =>
          traverse(fun, m, boundaries)

        case Assign(lhs, rhs) =>
          traverse(rhs, symCapabilities(lhs.symbol), boundaries)

        case If(cond, thenp, elsep) =>
          traverse(cond, m, boundaries)
          traverse(thenp, m, boundaries)
          traverse(elsep, m, boundaries)

        case LabelDef(name, params, rhs) =>
          traverse(rhs, m, boundaries)

        case TypeDef(mods, name, tparams, rhs) =>
          traverse(rhs, m, boundaries)

        case ValDef(mods, name, tpt, rhs) =>
          // TODO shouldn't we check the RHS with m? What is the annotation here?
          traverse(rhs, symCapabilities(tree.symbol), boundaries)

        case DefDef(mods, name, tparams, vparamss, tpt, rhs) if tree.symbol.isConstructor =>
        // do nothing

        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          //println(s"--- recurse $m def: ${tree.symbol}")

          val boundary1 = tree.symbol::boundaries
          traverse(rhs, Impure, boundary1)

        case Function(vparams, body) =>
          tree.symbol.addAnnotation(newLocalMarker(m))
          val boundary1 = tree.symbol :: boundaries
          // function bodies are always 1st class
          traverse(body, Impure, boundary1)


        // Look for SAM closure corresponding to `->`
        // We treat this case specially to make the closure argument @local
        /*
      {
        def apply$body(x: Int): Int = 2.*(x);
        @SerialVersionUID(0) final class $anonfun extends AnyRef with scala.util.escape.->[Int,Int] with Serializable {
          def <init>(): <$anon: Int => Int> = {
            $anonfun.super.<init>();
            ()
          };
          final override <synthetic> def apply(x: Int): Int = apply$body(x)
        };
        new $anonfun()
      }
      */
        case Block(List(
        bd@DefDef(bmods, bname, tparams, bvparamss, btpt, brhs), /* body */
        cd@ClassDef(mods, name, params,
        Template(parents, self, List(
        DefDef(_, _, _, _, _, _), /* <init> */
        DefDef(_, _, _, _, _, _)))) /* <apply> */
        ),
        Apply(Select(New(tpt), _ /*<init>*/), _))
          if tpt.symbol == cd.symbol
            && parents.exists(t => isSndFun(t.tpe))
        =>
          // find 2nd class function prototype
          val Some(base) = parents.find(t => isSndFun(t.tpe))

          // extract 'classiness' FIXME: proper sym lookup
          val mode = if (base.tpe.toString.startsWith("scala.util.escape.->*"))
            base.tpe.typeArgs(0) // e.g. `->*`[ReadOnly,File,Int]
          else // `->`
            Pure

          // add @local annotation to closure parameter
          val List(List(bvparam)) = bvparamss
          bvparam.symbol.addAnnotation(newLocalMarker(mode))

          // if this def is 1st class, take it as new boundary
          bd.symbol.addAnnotation(newLocalMarker(m))

          // go and check body
          traverse(brhs, Impure, bd.symbol :: boundaries)


        case Block(stats, expr) =>
          stats.foreach(s => traverse(s, m, boundaries))
          traverse(expr, m, boundaries)

        case This(qual) => // TODO: ok?

        case TypeTree() => // TODO: what?

        case New(tpt) => // TODO: what?

        case Typed(expr, tpt) => // TODO: what?
          traverse(expr, m, boundaries)

        case EmptyTree =>

        case Super(qual, mix) =>
          traverse(qual, m, boundaries)

        case Throw(expr) =>
          traverse(expr, m, boundaries)

        case Return(expr) =>
          traverse(expr, m, boundaries)

        case Import(expr, selectors) =>
          traverse(expr, Impure, boundaries)

        case Match(selector, cases) =>
          traverse(selector, m, boundaries)
          cases foreach { case cd@CaseDef(pat, guard, body) =>
            traverse(body, m, boundaries)
          }

        case Try(block, catches, finalizer) =>
          traverse(block, m, boundaries)
          catches foreach { case cd@CaseDef(pat, guard, body) =>
            traverse(body, m, boundaries)
          }
          traverse(finalizer, m, boundaries)

        case ClassDef(mods, name, params, impl) =>
          //println(s"--- recurse $m class: ${tree.symbol}")
          atOwner(tree.symbol) {
            traverse(impl, m, boundaries)
          }

        case Template(parents, self, body) =>
          atOwner(currentOwner) {
            // perform a crude RefChecks run:
            // subclasses are only allowed to _add_ @local annotations on
            // method parameters, not to remove them.

            // TODO: what about annotations on members directly,
            // not on method arguments?

            def checkOverride(pair: SymbolPair) = {
              val member = pair.low
              val other = pair.high

              def argModes(tpe: Type) = tpe match {
                case mt@MethodType(params, restpe) => params.map(symCapabilities)
                case _ => Nil
              }

              // member 2, other 1 is OK, but not vice versa
              val memberM = argModes(member.tpe)
              val otherM = argModes(other.tpe)

              def compare(a: Type, b: Type) = {
                val a1 = a.asSeenFrom(pair.rootType, a.typeSymbol.owner)
                val b1 = b.asSeenFrom(pair.rootType, b.typeSymbol.owner)
                val res = a1 <:< b1
                //println(s"$a1 <:<: $b1 = $res")
                res
              }

              val allOK = memberM.length == otherM.length && map2(otherM, memberM)(compare).forall(x => x)
              if (!allOK) {
                val fullmsg = "overriding " + pair.high.fullLocationString + " with " + pair.low.fullLocationString + ":\n" +
                  s"some @local annotations on arguments have been dropped"
                reporter.error(member.pos, fullmsg)
              }
              // require that either both have @local annotation on member or none
              // TODO: what is sensible here?
              if (symCapabilities(member) != symCapabilities(other)) {
                val fullmsg = "overriding " + pair.high.fullLocationString + " with " + pair.low.fullLocationString + ":\n" +
                  s"@local annotations on member do not match"
                reporter.error(member.pos, fullmsg)
              }
            }

            val opc = new overridingPairs.Cursor(tree.symbol)
            while (opc.hasNext) {
              if (!opc.high.isClass) checkOverride(opc.currentPair)
              opc.next()
            }

            // now check body (TODO: 2? 1?)
            body.foreach(s => traverse(s, Impure, boundaries))
          }

        case ModuleDef(mods, name, impl) =>
          traverse(impl, m, boundaries)

        case PackageDef(pid, stats) =>
          atOwner(tree.symbol) {
            stats.foreach(s => traverse(s, m, boundaries))
          }

        case _ =>
          println(s"don't know how to handle ${tree.getClass}")
      }
    }


    override def transform(tree: Tree): Tree = {
      // we start in an impure context, where everything can be captured
      traverse(tree, Impure, Nil)
      tree
    }
  }
}