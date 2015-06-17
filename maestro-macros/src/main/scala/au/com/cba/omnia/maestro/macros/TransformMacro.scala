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

import scala.reflect.macros.TypecheckException
import scala.reflect.macros.whitebox.Context

import scalaz._, Scalaz._

import org.apache.commons.lang.WordUtils

import com.twitter.scrooge.ThriftStruct

import au.com.cba.omnia.humbug.HumbugThriftStruct

import au.com.cba.omnia.maestro.core.transform.Transform

/**
  * Macro to automatically derive Transformations from one thrift struct to another.
  * 
  * It can take a variable number of rules that determine how to calculate the value for the
  * specified target field. Target fields that don't have specified rules are copied from a source
  * field with the same name. If there are no source fields with the same name, and no explicit
  * transformation exists to populate the target field, then the compilation will fail.
  */
object TransformMacro {
  type Result[T] = String \/ T

  def impl[A <: ThriftStruct : c.WeakTypeTag, B <: ThriftStruct : c.WeakTypeTag](c: Context)(
    transformations: c.Expr[(Symbol, A => _)]*
  ): c.Expr[Transform[A, B]] = {
    import c.universe.{Symbol => _, _}

    val srcType   = c.universe.weakTypeOf[A]
    val dstType   = c.universe.weakTypeOf[B]
    val srcFields = Inspect.fieldsMap[A](c)
    val dstFields = Inspect.fieldsMap[B](c)

    val in        = TermName(c.freshName)

    /** Fail compilation with nice error message. */
    def abort(msg: String) =
      c.abort(c.enclosingPosition, s"Can't create transform from $srcType to $dstType: $msg")

    /**
      * Parse the manual transformation rules.
      * 
      * Rules are of the form ('fieldName, A => $fieldType).
      */
    def parseTransform(transform: c.Expr[(Symbol, A => _)]): Result[(String, c.Tree)] = {
      transform.tree match {
        case q"((scala.Symbol.apply(${Literal(cons: Constant)}), $f))" => {
          val name  = cons.value.toString
          val field = TermName(name)

          dstFields.get(name) match {
            case None  => s"$name is not a member of $dstType.".left
            case Some(x) => {
              try {
                c.typecheck(q"$f: ($srcType => ${x.returnType})")
                (name, q"$f.apply($in)").right
              } catch {
                case TypecheckException(posn, msg) => s"Invalid type for transforming '$name: $msg.".left
              }
            }
          }
        }
        case q"($x, $f)" => abort(s"Expected target field in form 'name instead got $x.")
      }
    }

    /** Create code to copy fields with the same names from A to B.*/
    def copyTransform(name: String, method: MethodSymbol): Result[(String, c.Tree)] = {
      srcFields.get(name) match {
        case Some(src) if src.returnType == method.returnType =>
          (name, q"$in.${TermName(name)}").right
        case Some(src) =>
          s"$name has type ${src.returnType} in $srcType but $dstType expects ${method.returnType}.".left
        case None      =>
          s"$name is not a field of $srcType and no manual transformation was provided to construct it either.".left
      }
    }

    // Parses the manual transformation rules
    val (invalidManuals, manuals) =
      transformations
        .map(t => parseTransform(t))
        .toList
        .separate

    val manualDsts: Set[String] = manuals.map(_._1).toSet

    // Creates simple copy operations for any fields in the dst type that don't have a manual rule.
    val (invalidDefaults, defaults)   =
      dstFields
        .filterKeys(n => !manualDsts.contains(n))
        .toList
        .map { case (n, f) => copyTransform(n, f) }
        .separate

    // Checks that the manual rules don't try and transform the same field multiple times.
    val multipleTransforms = manuals.groupBy(_._1).toList.filter(_._2.length != 1).map(_._1)
    if (!multipleTransforms.isEmpty) {
      abort(s"""Can't transform ${multipleTransforms.mkString(",")} multiple times.""")
    }

    // Deal with any errors around the transformation rules
    val invalids = invalidManuals ++ invalidDefaults
    if (!invalids.isEmpty) {
      abort(invalids.mkString("\n"))
    }

    val combined = Inspect.constructNamed[B](c)(defaults ++ manuals)
    val result   = q"""
      import au.com.cba.omnia.maestro.core.transform.Transform
      Transform[$srcType, $dstType](($in: $srcType) => $combined)
    """

    c.Expr[Transform[A, B]](result)
  }
}
