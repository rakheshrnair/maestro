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

import com.twitter.scrooge.ThriftStruct

import au.com.cba.omnia.humbug.HumbugThriftStruct

object Inspect {
  val ProductField = """_(\d+)""".r

  case class FieldDefinition[T, G] (
    name: String,
    index: Int,
    type: T,
    getter: G
  )

  def allFields[A <: ThriftStruct : c.WeakTypeTag](c: Context)(typ: c.universe.Type): List[FieldDefinition] = {
    import c.universe.{Symbol => _, _}
    /** Gets all the fields of a Thrift struct sorted in order of definition, for any type assumed to be ThriftStruct.*/
    def fields(c: Context): List[(String, c.universe.MethodSymbol)] = {
      import c.universe._

      val typ = c.universe.weakTypeOf[A]
      val productFieldRegex = """_(\d+)""".r

      /** Gets all the `_1` style getters for a thrift struct in numerical order.*/
      val methodSymbols =
        typ.members.toList.map(member => (member, member.name.toString)).collect({
          case (member, productFieldRegex(n)) => (member.asMethod, n.toInt)
        }).sortBy(_._2).map(_._1)

      val names =
        if (typ <:< c.universe.weakTypeOf[HumbugThriftStruct]) {
          // Get the fields in declaration order
          typ.declarations.sorted.toList.collect {
            case sym: TermSymbol if sym.isVar => sym.name.toString.trim
          }
        } else {
          typ.typeSymbol.companionSymbol.typeSignature
            .member(newTermName("apply")).asMethod.paramss.head.map(_.name.toString)
        }

      methodSymbols.zip(names)
    }

    val srcType       = c.universe.weakTypeOf[A]
    val humbugTyp     = c.universe.weakTypeOf[HumbugThriftStruct]

    val dstFields     = fields[A](c)
    val expectedTypes = dstFields.map { case (f, n) => (n, f.returnType) }

    //val in  = newTermName(c.fresh)

    // def humbugGen(typ: Type, args: List[(String, Type)]): Tree = {
    //   // https://issues.scala-lang.org/browse/SI-8425
    //   //val out = newTermName(c.fresh) // Requires scala-2.11
    //   val out = newTermName("out")

    //   def mkInner(args: List[(String, Type)]): Tree = {
    //     if (args.isEmpty) {
    //       q"$out"
    //     } else {
    //       val (n, t) = args.head
    //       val nn     = newTermName(n)
    //       val ni     = Ident(nn)
    //       val inner  = mkInner(args.tail)
    //       q"""arbitrary[$t] flatMap { $ni: $t => $out.$nn = $ni; ..$inner }"""
    //     }
    //   }

    //   val inner = mkInner(args)

    //   q"""
    //     val $out = new $typ
    //     ..$inner
    //   """
    // }

    // def scroogeGen(typ: Type, args: List[(String, Type)]): Tree = {
    //   def mkNew(vals: List[c.Tree]) = {
    //     val companion = typ.typeSymbol.companionSymbol
    //     Apply(Select(Ident(companion), newTermName("apply")), vals)
    //   }

    //   def mkInner(args: List[(String, Type)], terms: List[Tree]): Tree = {
    //     if (args.isEmpty) {
    //       mkNew(terms)
    //     } else {
    //       val (n, t) = args.head
    //       val nn     = Ident(newTermName(n))
    //       val inner  = mkInner(args.tail, terms :+ nn)
    //       q"""arbitrary[$t] flatMap { $nn: $t => ..$inner }"""
    //     }
    //   }

    //   mkInner(args, List())
    // }

    // val body = srcType match {
    //   case t if t <:< humbugTyp => humbugGen(srcType, expectedTypes)
    //   case _                    => scroogeGen(srcType, expectedTypes)
    // }

    // val result = q"""
    //   import org.scalacheck.Arbitrary
    //   import org.scalacheck.Arbitrary.arbitrary
    //   import org.scalacheck.Gen
    //   Arbitrary[$srcType]($body)
    // """

    // c.Expr[Arbitrary[A]](result)
  }

  

   

  }

  /** Gets all the `_1` style getters and their number for a thrift struct in numerical order.*/
  def indexed[A <: ThriftStruct: c.WeakTypeTag](c: Context): List[(c.universe.MethodSymbol, Int)] = 
    indexedUnsafe(c)(c.universe.weakTypeOf[A])

  /** Same as indexed but for any type where the type is assumed to be ThriftStruct.*/
  def indexedUnsafe(c: Context)(typ: c.universe.Type): List[(c.universe.MethodSymbol, Int)] = {
    typ.members.toList.map(member => (member, member.name.toString)).collect({
      case (member, ProductField(n)) =>
        (member.asMethod, n.toInt)
    }).sortBy(_._2)
  }

  /** Gets all the fields of a Thrift struct sorted in order of definition.*/
  def fields[A <: ThriftStruct: c.WeakTypeTag](c: Context): List[(c.universe.MethodSymbol, String)] =
    fieldsUnsafe(c)(c.universe.weakTypeOf[A])

  /** Does a type derive from ThriftStruct? */
  def isThriftType[A: c.WeakTypeTag](c:Context): Boolean =
    c.universe.weakTypeOf[A] <:< c.universe.weakTypeOf[ThriftStruct]

  /** Abort unless A derives from ThriftStruct. */
  def ensureThriftType[A: c.WeakTypeTag](c: Context): Unit =
    // Since type bounds on macro implementations don't currently prevent the macro from expanding
    // we need this workaround to avoid materializing encoders for primitives. 
    if (!isThriftType[A](c))
      c.abort(c.enclosingPosition, s"${c.universe.weakTypeOf[A].toString} does not derive ThriftStruct.")

  /** Same as fields but for any type where the type is assumed to be ThriftStruct.*/
  def fieldsUnsafe(c: Context)(typ: c.universe.Type): List[(c.universe.MethodSymbol, String)] = {
    import c.universe._

    val fields =
      if (typ <:< c.universe.weakTypeOf[HumbugThriftStruct]) {
        // Get the fields in declaration order
        typ.decls.sorted.toList.collect {
          case sym: TermSymbol if sym.isVar => sym.name.toString.trim.capitalize
        }
      } else
        typ.typeSymbol.companion.typeSignature
          .member(TermName("apply")).asMethod.paramLists.head.map(_.name.toString.capitalize)

    methodsUnsafe(c)(typ).zip(fields)
  }

  /** Gets all the `_1` style getters for a thrift struct in numerical order.*/
  def methods[A <: ThriftStruct: c.WeakTypeTag](c: Context): List[c.universe.MethodSymbol] =
    indexed(c).map({ case (method, _) => method })

  /** Same as methods but for any type where the type is assumed to be ThriftStruct.*/
  def methodsUnsafe(c: Context)(typ: c.universe.Type): List[c.universe.MethodSymbol] =
    indexedUnsafe(c)(typ).map({ case (method, _) => method })
}
