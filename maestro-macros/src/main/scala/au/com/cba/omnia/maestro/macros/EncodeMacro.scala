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

import au.com.cba.omnia.maestro.core.codec.Encode

/**
  * Converts a thrift struct into a list of its fields.
  * 
  * Each field is encoded as the [[Val]] corresponding to its type.
  */
object EncodeMacro {
  def impl[A <: ThriftStruct: c.WeakTypeTag](c: Context): c.Expr[Encode[A]] = {
    import c.universe._
    Inspect.ensureThriftType[A](c)

    val typ       = weakTypeOf[A]
    val allFields = Inspect.allFields[A](c)

    def encode(xs: List[MethodSymbol]): List[Tree] = xs.map { x =>
      q"Encode.encode[${x.returnType}](none, a.${x.getter})"
    }

    val fields = q"""List(..${encode(allFields)}).flatten"""

    val r = q"""
       import au.com.cba.omnia.maestro.core.codec.Encode
       Encode((none: String, a: $typ) => { $fields })
    """
    c.Expr[Encode[A]](r)
  }
}
