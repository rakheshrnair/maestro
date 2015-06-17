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
import scala.annotation.StaticAnnotation

import com.twitter.scrooge.ThriftStruct

class body(tree: Any) extends StaticAnnotation

/**
  * A macro that generates a `Field` for every field in the thrift struct. All the fields are
  * members of FieldsWrapper.
  */
object FieldsMacro {
  def impl[A <: ThriftStruct: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val typ        = c.universe.weakTypeOf[A]
    val entries    = Inspect.info[A](c)
    val companion  = typ.typeSymbol.companion
    val nameGetter = TermName("name")
    
    val fields = entries.map { case (i, name, method) =>
      val field   = name.capitalize
      val srcName = q"""$companion.${TermName(field + "Field")}.$nameGetter"""
      val get     = q"""au.com.cba.omnia.maestro.core.data.Accessor[${method.returnType}]($i)"""
      val fld     = q"""au.com.cba.omnia.maestro.core.data.Field[$typ, ${method.returnType}]($srcName, $get)"""
        
      q"""val ${TermName(field)} = $fld"""
    }

    val refs = entries.map { case (_, field, _) => q"${TermName(field.capitalize)}" }
    val r    =q"class FieldsWrapper { ..$fields; def AllFields = List(..$refs) }; new FieldsWrapper {}"
    c.Expr(r)
  }
}
