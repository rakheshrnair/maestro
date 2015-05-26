#   Copyright 2014 Commonwealth Bank of Australia
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

#@namespace scala au.com.cba.omnia.maestro.innercore.thrift.humbug

struct Exhaustive {
  1:           string  myString
  2:           bool    myBoolean
  3:           i32     myInt
  4:           i64     myLong
  5:           double  myDouble
  6: optional  i32     myOptInt
  7: optional  string  myOptString
}
