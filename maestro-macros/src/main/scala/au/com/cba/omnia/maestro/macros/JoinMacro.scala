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

import scala.reflect.macros.whitebox.Context

import scalaz.{\/, -\/, \/-, NonEmptyList}
import scalaz.syntax.either._

import org.apache.commons.lang.WordUtils

import com.twitter.scrooge.ThriftStruct

import au.com.cba.omnia.humbug.HumbugThriftStruct

import au.com.cba.omnia.maestro.core.transform.Join

/**
  * Macro to automatically derive transformations from a tuple of thrift structs
  * to one resulting output thrift struct.
  *
  * The macro will generate a `Join` value which matches input fields to
  * output fields if they have the same name and conforming types. If multiple
  * input fields match, they must have the same values at runtime or the `Join`
  * value will throw an exception.
  *
  * Compilation will succeed when:
  * - The input type is a product (e.g. tuple) of thrift structures.
  * - Every field in the output thrift struct has matching fields (fields with
  *   the same name) amongst the input thrift structs.
  * - Every matching input field's type conforms to the type of it's output field.
  */
object JoinMacro {
  type Fails[A] = String \/ A

  def impl[A <: Product : c.WeakTypeTag, B <: ThriftStruct : c.WeakTypeTag](c: Context)
      : c.Expr[Join[A, B]] = {
    import c.universe._

    // thrift types and fields
    case class ThriftField(name: TermName, typ: Type)
    case class ThriftType(typ: Type, fields: List[ThriftField])

    // A thrift type inside the input product. name is the name of the input product field
    case class ThriftInput(name: TermName, thrift: ThriftType)

    // a mapping from fields in the input thrift product to a field in the destination thrift type
    case class Mapping(sources: NonEmptyList[(ThriftInput, ThriftField)], dest: ThriftField)

    def abort(msg: String) =
      c.abort(c.enclosingPosition, msg)

    def thriftType(typ: Type): ThriftType =
      ThriftType(typ, Inspect.infoUnsafe(c)(typ).map { case (_, name, method) =>
        ThriftField(TermName(WordUtils.uncapitalize(name)), method.typeSignatureIn(typ))
      })

    // get the thrift structures from the input type, or fail if the input type is not a product of thrift structs
    def thriftInputs(inTyp: Type): Fails[List[ThriftInput]] = {
      val thriftInputs = inTyp.decls collect {
        case sym: TermSymbol if sym.isVal && sym.isCaseAccessor => {
          val typ = sym.typeSignatureIn(inTyp)
          if (typ <:< weakTypeOf[ThriftStruct])
            ThriftInput(TermName(sym.name.toString.trim), thriftType(typ)).right
          else
            s"$typ is not a thrift struct".left
        }
      }
      collectErrors(thriftInputs, errs => s"Join requires a product of thrift structs: ${errs.mkString(", ")}")
    }

    // get the mappings from input fields to output fields,
    // or fail if there are not mappings for each output field or some matching fields have non-conforming types
    def mappingsFor(outputFields: List[ThriftField], inputs: List[ThriftInput]): Fails[List[Mapping]] = {
      // dumb simple algorithm, could probably be improved if noticeably slow
      def mappingsForField(dest: ThriftField) = collectErrors(
        msg     = errs => s"${dest.name} is not compatible with ${errs.mkString(" and  ")}",
        eithers = for {
          input  <- inputs
          source <- input.thrift.fields
          if source.name == dest.name
        } yield {
          if (source.typ <:< dest.typ) (input, source).right
          else                         s"${input.thrift.typ.typeSymbol.name}.${source.name}: ${source.typ}".left
        }
      ) match {
        case -\/(msg)          => msg.left
        case \/-(List())       => s"${dest.name} has no input field".left
        case \/-(head :: tail) => Mapping(NonEmptyList(head, tail: _*), dest).right
      }

      val candidates = outputFields.map(mappingsForField(_))
      collectErrors(candidates, errs => s"Join requires output fields to have matching compatible input fields: ${errs.mkString(", ")}")
    }

    /* If there is one source, produces code like (modulo whitespace, fully qualified paths, and variable names):
     *
     *   input._1.field1
     *
     * If there are multiple sources, produces code like (modulo whitespace, fully qualified paths, and variable names):
     *
     *   if (input._1.field1 == input._2.field1 && input._1.field1 == input._3.field1) {
     *     input._1.field1
     *   } else {
     *     val details = List("Thrif1.field1 = " + input._1.field, "Thrift2.field1 = " + input._2.field1, "Thrift3.field1 = " + input._3.field1).mkString(", ")
     *     throw new IllegalArgumentException("Ambiguous source values: " + details)
     *   }
     */
    def unambSource(inpTerm: TermName, mapping: Mapping) = {
      val head@(input1, field1) = mapping.sources.head
      val headValue = q"""$inpTerm.${input1.name}.${field1.name}"""

      mapping.sources.tail match {
        case List() => headValue

        case tail   => {
          val tailValues = tail map { case (input, field) =>
            q"""$inpTerm.${input.name}.${field.name}"""
          }
          val errorDetails = (head :: tail) map { case (input, field) =>
            val strLit = s"${input.thrift.typ.typeSymbol.name}.${field.name} = "
            q"""$strLit + $inpTerm.${input.name}.${field.name}"""
          }
          val condition = tailValues
            .map(tailValue => q"""$headValue == $tailValue""")
            .reduce((condition1,condition2) => q"""$condition1 && $condition2""")

          q"""
          if ($condition) {
            $headValue
          } else {
            val errorDetails = List(..$errorDetails).mkString(", ")
            throw new IllegalArgumentException("Ambiguous source values: " + errorDetails)
          }
          """
        }
      }
    }

    /* Produces code like (modulo whitespace, fully qualified paths, variable names, humbug vs. scrooge, and multiple sources for one field):
     *
     *   Join[(Thrift1, Thrift2), Joined](
     *     (input: (Thrift1, Thrift2)) => {
     *       Joined(
     *         field1 = input._1.field1,
     *         field2 = input._1.field2,
     *         field3 = input._2.field3
     *       )
     *     }
     *   )
     */
    def joinTree(inputType: Type, output: ThriftType, inputs: List[ThriftInput], mappings: List[Mapping]): Tree = {
      val inpTerm = TermName(c.freshName("input"))
      val struct  = Inspect.constructNamed[B](c)(
        mappings.map(m => (m.dest.name.toString, unambSource(inpTerm, m)))
      )

      q"""
      au.com.cba.omnia.maestro.core.transform.Join[$inputType, ${output.typ}](
        ($inpTerm: $inputType) => {
          $struct
        }
      )
      """
    }

    val inputType = weakTypeOf[A]
    val output    = thriftType(weakTypeOf[B]) // we know this is a thrift type because of type signature

    val tree      = for {
      inputs     <- thriftInputs(inputType)
      mappings   <- mappingsFor(output.fields, inputs)
    } yield joinTree(inputType, output, inputs, mappings)

    c.Expr[Join[A, B]](tree.fold(msg => abort(msg), identity))
  }

  def collectErrors[A](eithers: Iterable[Fails[A]], msg: List[String] => String): Fails[List[A]] =
    eithers.foldRight[List[String] \/ List[A]](List.empty[A].right) {
      case (\/-(ok),  \/-(oks))  => (ok :: oks).right
      case (-\/(err), \/-(oks))  => List(err).left
      case (\/-(ok),  -\/(errs)) => errs.left
      case (-\/(err), -\/(errs)) => (err :: errs).left
    }.leftMap(msg(_))
}
