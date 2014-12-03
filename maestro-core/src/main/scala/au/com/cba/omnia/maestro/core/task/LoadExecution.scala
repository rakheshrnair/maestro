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

package au.com.cba.omnia.maestro.core
package task

import com.twitter.scalding.{Execution, ExecutionCounters, TypedPipe}

import com.twitter.scrooge.ThriftStruct

import au.com.cba.omnia.maestro.core.codec.{Decode, Tag}
import au.com.cba.omnia.maestro.core.clean.Clean
import au.com.cba.omnia.maestro.core.filter.RowFilter
import au.com.cba.omnia.maestro.core.scalding.StatKeys
import au.com.cba.omnia.maestro.core.time.TimeSource
import au.com.cba.omnia.maestro.core.validate.Validator

/** Information about a Load */
sealed trait LoadInfo {
  def continue: Boolean = this match {
    case EmptyLoad            => false
    case LoadFailure(_, _, _) => false
    case LoadSuccess(_, _, _) => true
  }
}

/** The Load had no new data to process */
case object EmptyLoad extends LoadInfo

/** The number of rows which failed to load exceeded our error threshold */
case class LoadFailure(read: Long, written: Long, failed: Long) extends LoadInfo

/** The number of rows which failed to load was within acceptable limits */
case class LoadSuccess(read: Long, written: Long, failed: Long) extends LoadInfo

/** Factory methods for `LoadInfo` */
object LoadInfo {
  def fromCounters(counters: ExecutionCounters, errorThreshold: Double): LoadInfo = {
    // If there is no data, nothing will be read in and this counter won't be set
    val read    = counters.get(StatKeys.tuplesRead).getOrElse(0l)
    // If all values fail nothing will be written out and this counter won't be set
    val written = counters.get(StatKeys.tuplesWritten).getOrElse(0l)
    // If all values pass nothing will be written out and this counter won't be set
    val failed  = counters.get(StatKeys.tuplesTrapped).getOrElse(read - written)

    if (read == 0) EmptyLoad
    else if (failed / read.toDouble >= errorThreshold) LoadFailure(read, written, failed)
    else LoadSuccess(read, written, failed)
  }
}

/** Executions for load tasks */
trait LoadExecution {
  /**
    * Loads the supplied text files and converts them to the specified thrift struct.

    * The operations performed are:
    *  1. Append a time field to each line using the provided time source.
    *  1. Split each line into columns/fields using the provided delimiter.
    *  1. Apply the provided filter to each list of fields.
    *  1. Clean each field using the provided cleaner.
    *  1. Convert the list of fields into the provided thrift struct. Option fields with the `none`
    *     value or empty strings (expect for optional string fields) are set to `None`.
    *  1. Validate each struct.
    */
  def load[A <: ThriftStruct : Decode : Tag : Manifest](
    delimiter: String, sources: List[String], errors: String, timeSource: TimeSource, clean: Clean,
    validator: Validator[A], filter: RowFilter, none: String, errorThreshold: Double = 0.05
  ): Execution[(TypedPipe[A], LoadInfo)] =
    LoadHelper.execution[A](
      errorThreshold,
      LoadHelper.load[A](delimiter, sources, errors, timeSource, clean, validator, filter, none)
    )

  /**
    * Same as `load` but also appends a unique key to each line. The last field of `A`
    * needs to be set aside to receive the key as type string.
    *
    * The key is generated by:
    *  1. Creating a random 4 byte seed for each load
    *  1. For each file hashing the seed and the path and then taking the last 8 bytes.
    *  1. Hashing each line and taking 12 bytes, taking the map task number (current slice number) and the offset into the file.
    *  1. Concatenating the file hash from 2, the slice number, byte offset and line hash
    *
    * This produces a 256 bit key.
    */
  def loadWithKey[A <: ThriftStruct : Decode : Tag : Manifest](
      delimiter: String, sources: List[String], errors: String, timeSource: TimeSource, clean: Clean,
      validator: Validator[A], filter: RowFilter, none: String, errorThreshold: Double = 0.05
  ): Execution[(TypedPipe[A], LoadInfo)] =
    LoadHelper.execution[A](
      errorThreshold,
      LoadHelper.loadWithKey[A](delimiter, sources, errors, timeSource, clean, validator, filter, none)
    )

  /** Same as `load` but uses a list of column lengths to split the string rather than a delimeter. */
  def loadFixedLength[A <: ThriftStruct : Decode : Tag : Manifest](
    lengths: List[Int], sources: List[String], errors: String, timeSource: TimeSource,
      clean: Clean, validator: Validator[A], filter: RowFilter, none: String, errorThreshold: Double = 0.05
  ): Execution[(TypedPipe[A], LoadInfo)] =
    LoadHelper.execution[A](
      errorThreshold,
      LoadHelper.loadFixedLength[A](lengths, sources, errors, timeSource, clean, validator, filter, none)
    )
}

private object LoadHelper extends Load {
  def execution[A](errorThreshold: Double, pipe: TypedPipe[A]): Execution[(TypedPipe[A], LoadInfo)] = {
    pipe
      .forceToDiskExecution
      .getAndResetCounters
      .map { case (pipe, counters) => (pipe, LoadInfo.fromCounters(counters, errorThreshold)) }
  }
}
