//   Copyright 2014 Commonwealth Bank of Australia
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

package au.com.cba.omnia.maestro.macros

import scala.annotation.StaticAnnotation
import scala.reflect.macros.TypecheckException
import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.collection.immutable.StringOps

import scalaz.Scalaz._
import scalaz.NonEmptyList
import scalaz.\/

object automap2 {
  type AutomapError = String

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    /** Abort macro expansion with an error message. */
    def fail(reason: String) = c.abort(c.enclosingPosition, reason)


    /** Ugly hack to convert the type that is in `tree` to an instance of [[Type]]. */
    def resolveType(tree: Tree): Type = {
      // The following obscure construction seems to be the simplest way of reliably forcing
      // resolution of an externally-defined class and its companion class.
      val q"{ class $_[$_] { type FakeType = $typ; () } }" =
        c.typecheck( q"{ class Dummy[T] { type FakeType = $tree; () } }" )

      typ.tpe
    }

    /** Creates a map with the code to get the value for each field from the instance of `typ` bound to `name`. */ 
    def createGetters(name: TermName, typ: Type): Map[String, Tree] =
      Inspect.fieldsMapUnsafe(c)(typ).mapValues(v => q"$name.$v")

    /*
     * For multiple sources, produces code like (modulo whitespace, fully qualified paths, and variable names):
     *
     *  {{{
     *  if (x.field1 == y.field1 && x.field1 == z.field1) x.field1
     *  else
     *     val details = 
     *     throw new IllegalArgumentException("Ambiguous source values for field1: " + List("x.field1 = " + x.field1, "y.field1 = " + y.field1, "z.field1 = " + z.field1).mkString(", "))
     *  }}}
     */
    def unambiguateValues(name: String, values: NonEmptyList[(TermName, Tree)]): Tree = {
      val (hn, ht)  = values.head
      val condition =
        values.tail
          .map(t => q"$ht == ${t._2}")
          .reduce((c1, c2) => q"$c1 && $c2")

      val errorDetails = values.map { case (src, v) =>
        val srcString = s"$src.$name = "
        q"$srcString + $v"
      }.list

      val exceptionMsg = s"Ambiguous source values for $name: "

      q"""
        if ($condition) $ht
        else
          throw new IllegalArgumentException($exceptionMsg + List(..$errorDetails).mkString(", "))
      """
    }


    /** Pull apart the source object, pad it out, put it back together. */
    def mkMapper(src: Tree): Seq[AutomapError] \/ Tree = {
      val q"def $name (..$srcArgs): $srcTo = $srcBody" = src
      val srcRules = srcBody match {
        case Block(xs, x) => x :: xs
        case _            => Nil
      }

      val dst = resolveType(srcTo)
      val dstFields = Inspect.fieldsMapUnsafe(c)(dst)
      val srcs: List[(TermName, Type)] = 
        srcArgs.map { case q"$_ val $n: $t = $_" => (n, resolveType(t)) }

      val srcFields: List[(TermName, Map[String, MethodSymbol])] =
        srcs.map { case (n, t) => (n, Inspect.fieldsMapUnsafe(c)(t)) }


      // Extract all the manual code specified for each field.
      val (invalidOverrides, validOverrides) =
        srcRules
          .map { case q"$x := $expr" =>
            dstFields.get(x.toString).cata(f =>
              try {
                val params = srcs.map { case (n, t) => q"$n: $t" }
                val paramTypes = srcs.map(_._2)
                val wrapper = q"(..$params) => $expr"
                c.typecheck(q"$expr: (..$paramTypes => ${f.returnType})")
                (x.toString, expr).right
              } catch {
                case TypecheckException(posn, msg) => s"Invalid type for transforming '$name: $msg.".left
              },
              s"$x is not a member of $dst".left
            )
        }.separate


      val overrides: Map[String, Tree] = validOverrides.toMap

      val multipleOverrides =
        srcRules.map { case q"$x := $_" => x }.groupBy(identity).collect {
          case (k, vs) if vs.length > 1 => k
        }

      // Determine all the fields that don't have manual rules.
      val remainingFields: Map[String, MethodSymbol] =
        dstFields.filterKeys(k => overrides.contains(k))
      //Tries and matches each of the remaining fields with fields from the inputs based on name and type.
      val defaults: Map[String, List[(TermName, Tree)]] = remainingFields.map { case (name, method) =>
        (name, srcFields.flatMap { case (src, fields) => 
          fields.get(name)
            .filter(_.returnType == method.returnType)
            .map(srcField => (src, q"$src.$srcField"))
        })
      }

      // Identify all fields for which we don't have manual rules or matching fields in the sources.
      val missing = defaults.toList.collect { case (n, List()) => n }

      // Code to assign a value from the input sources.
      // Where multiple sources had matched a particular field in type and name we
      // unambiguate the value at run time.
      val defaultsCode: Map[String, Tree] = defaults.collect {
        case (n, List((_, t))) => (n, t)
        case (n, h :: ts)      => (n, unambiguateValues(n, NonEmptyList.nel(h, ts)))
      }



      ???
    }

    annottees
      .map(_.tree)
      .map {
      case (x: DefDef) => {
        mkMapper(x).fold(errors => fail(errors.mkString("\n")), succ => c.Expr[Any](succ))
      }
      case _           => fail("Automap annottee must be method accepting thrift structs and returning one.")
    }.headOption.getOrElse(fail("No annottees found"))
  }
}
