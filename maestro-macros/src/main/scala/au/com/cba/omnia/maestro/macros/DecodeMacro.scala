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

import com.twitter.scrooge._

import au.com.cba.omnia.humbug.HumbugThriftStruct

import au.com.cba.omnia.maestro.core.codec._

/** Creates a custom Decode for the given thrift struct. */
object DecodeMacro {
  def impl[A <: ThriftStruct: c.WeakTypeTag](c: Context): c.Expr[Decode[A]] = {
    import c.universe._

    Inspect.ensureThriftType[A](c)
    
    val typ        = c.universe.weakTypeOf[A]
    val stringType = weakTypeOf[String]
    val typeName   = typ.toString
    val members    = Inspect.info[A](c).map { case (i, _, f) => (i, f) }
    val size       = members.length

    def decodeUnknownSource(xs: List[(Int, MethodSymbol)]) = q"""
      if (source.length < $size) {
        DecodeError[(List[String], Int, $typ)](source, position, NotEnoughInput($size, $typeName))
      } else {
        val fields = source.take($size).toArray
        var index  = -1
        var tag    = "" 
        try {
          val result = ${Inspect.constructIndexed[A](c)(decodeUnknowns(xs))}
          DecodeOk((source.drop($size), position + $size, result))
        } catch {
          case NonFatal(e) => DecodeError[(List[String], Int, $typ)](
            source.drop(index - position),
            position + index,
            ParseError(fields(index), tag, That(e))
          )
        }
      }
    """

    def decodeUnknowns(xs: List[(Int, MethodSymbol)]): List[Tree] = xs.map { case (i, x) =>
      val index  = i - 1

      MacroUtils.optional(c)(x.returnType).map { param =>
        if (param == stringType)
          q"""
            if (fields($index) == none) Option.empty[String]
            else                        Option(fields($index))
          """
        else {
          val method = TermName("to" + param)
          val tag    = s"Option[$param]"
          q"""{
            tag   = $tag
            index = $index

            if (fields(index).isEmpty || fields(index) == none)
              Option.empty[$param]
            else
              Option(fields(index).$method)
          }"""
        }
      } getOrElse {
        if (x.returnType == stringType)
          q"fields($index)"
        else {
          val method = TermName("to" + x.returnType)
          val tag    = x.returnType.toString

          q"""{
            tag   = $tag
            index = $index
            fields(index).$method
          }"""
        }
      }
    }

    val combined = q"""
      import au.com.cba.omnia.maestro.core.codec.Decode
      Decode((none, source, position) => {
        import scala.util.control.NonFatal
        import scalaz.\&/.That
        import au.com.cba.omnia.maestro.core.codec.{DecodeOk, DecodeError, DecodeResult, ParseError, NotEnoughInput}
  
        ${decodeUnknownSource(members)}
      })
    """

    c.Expr[Decode[A]](combined)
  }
}
