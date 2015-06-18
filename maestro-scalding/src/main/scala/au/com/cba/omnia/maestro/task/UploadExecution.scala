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

package au.com.cba.omnia.maestro.task

import java.io.{BufferedReader, FileReader, File}

import scala.util.matching.Regex

import org.slf4j.{Logger, LoggerFactory}

import org.apache.commons.io.input.ReversedLinesFileReader

import com.twitter.scalding._

import au.com.cba.omnia.omnitool.Result

import au.com.cba.omnia.answer.DBConfig
import au.com.cba.omnia.etl.controller.LoadController

import au.com.cba.omnia.maestro.core.upload._
import au.com.cba.omnia.maestro.scalding.ConfHelper
import au.com.cba.omnia.maestro.scalding.ExecutionOps._

/** Information about an upload */
case class UploadInfo(files: List[String]) {
  /** Should we continue the load? */
  def continue: Boolean = !files.isEmpty

  /** Returns a non-emtpy list of sources if we should continue the load.
    * Fails otherwise.
    */
  def withSources: Execution[List[String]] = Execution.from(
    if (continue) files else throw new Exception("No files to upload")
  )
}

/**
  * Configuration options for upload.
  *
  * Files are copied from the local ingestion path to the three destination paths
  * (HDFS landing path, local archive path, and HDFS archive path). When files
  * are copied to a destination path the effective date of the upload are added
  * to the path. For instance: the actual location of files on HDFS might be
  * `\$hdfsLandingPath/<year>/<month>/<day>/<originalFileName>`.
  *
  * @param localIngestPath  The local ingestion path where source files are found.
  * @param hdfsLandingPath  The HDFS landing path where files are copied to.
  * @param localArchivePath The local archive path where files are archived.
  * @param hdfsArchivePath  The HDFS archive path, where files are also archived.
  * @param tablename        The table name or file name in database or project.
  * @param filePattern      The file pattern, used to identify files to be uploaded.
  *                         The default value is `{table}?{yyyyMMdd}*`.
  * @param controlPattern   The regex which identifies control files.
  *                         The default value is [[au.com.cba.omnia.maestro.core.upload.ControlPattern.default]].
  */
case class UploadConfig(
  localIngestPath: String,
  hdfsLandingPath: String,
  localArchivePath: String,
  hdfsArchivePath: String,
  tablename: String,
  filePattern: String = "{table}?{yyyyMMdd}*",
  controlPattern: Regex = ControlPattern.default
){
  def prettyPrint =
    s"""TableName        = ${tablename},
       |filePattern      = ${filePattern},
       |controlPattern   = ${controlPattern},
       |localIngestPath  = ${localIngestPath},
       |localArchivePath = ${localArchivePath},
       |hdfsArchivePath  = ${hdfsArchivePath},
       |hdfsLandingPath  = ${hdfsLandingPath}
       |""".stripMargin
}

/**
  * Push source files to HDFS using [[upload]] and archive them.
  *
  * See the example at `au.com.cba.omnia.maestro.example.CustomerExecution`.
  *
  * In order to run map-reduce jobs, we first need to get our data onto HDFS.
  * [[upload]] copies data files from the local machine onto HDFS and archives
  * the files. The user can also use [[findSources]] and [[uploadSources]],
  * which allows them to inspect the data files before uploading them.
  *
  * The user can run [[parseHeader]], [[parseTrailer]], and [[parseHT]] on
  * data files to retrieve and validate their metadata.
  */
trait UploadExecution {
  /**
    * Pushes source files onto HDFS and archives them locally.
    *
    * `upload` expects data files intended for HDFS to be placed in the local
    * ingestion path. `upload` processes all data files that match a given file
    * pattern. The file pattern format is explained below.
    *
    * Each data file will be copied into the HDFS landing path. Data files are
    * also compressed and archived on the local machine and on HDFS.
    *
    * Some files placed on the local machine are control files. These files
    * are not intended for HDFS and are ignored by `upload`. `upload` will log
    * a message whenever it ignores a control file.
    *
    * When an error occurs, `upload` stops copying files immediately. Once the
    * cause of the error has been addressed, `upload` can be run again to copy
    * any remaining files to HDFS. `upload` will refuse to copy a file if that
    * file or it's control flags are already present in HDFS. If you
    * need to overwrite a file that already exists in HDFS, you will need to
    * delete the file and it's control flags before `upload` will replace it.
    *
    * The file pattern is a string containing the following elements:
    *  - Literals:
    *    - Any character other than `{`, `}`, `*`, `?`, or `\` represents itself.
    *    - `\{`, `\}`, `\*`, `\?`, or `\\` represents `{`, `}`, `*`, `?`, and `\`, respectively.
    *    - The string `{table}` represents the table name.
    *  - Wildcards:
    *    - The character `*` represents an arbitrary number of arbitrary characters.
    *    - The character `?` represents one arbitrary character.
    *  - Date times:
    *    - The string `{<timestamp-pattern>}` represents a JodaTime timestamp pattern.
    *    - `upload` only supports certain date time fields:
    *      - year (y),
    *      - month of year (M),
    *      - day of month (d),
    *      - hour of day (H),
    *      - minute of hour (m), and
    *      - second of minute (s).
    *
    * Some example file patterns:
    *  - `{table}{yyyyMMdd}.DAT`
    *  - `{table}_{yyyyMMdd_HHss}.TXT.*.{yyyyMMddHHss}`
    *  - `??_{table}-{ddMMyy}*`
    *
    * @param config Upload configuration: [[UploadConfig]].
    *
    * @return List of copied hdfs files.
    */
  def upload(config: UploadConfig): Execution[UploadInfo] =
    UploadEx.matcher(config).flatMap(UploadEx.uploader(config, _))

  def uploadWithLogging(config: UploadConfig,
    loadController: LoadController, ctlDBConfig: DBConfig
  ): Execution[UploadInfo] = for {
    result <- UploadEx.matcher(config).flatMap(UploadEx.uploader(config, _))
    Execution.fromResult(loadController.logLoadRunStep(run, "UploadStep", n, "Upload complete").run(ctlDBConfig))
  } yield result

  /**
    * Attempt to parse the first line of the file into a header data type.
    *
    * The default header parser is described in [[parseHT]].
    * The user can supply their own parsing function and header data type if necesary.
    *
    * @tparam H Header data type
    *
    * @param parseH Function to parse header
    * @param file   Input source file
    *
    * @return Header data
    */
  def parseHeader[H](parseH: String => Result[H] = HeaderParsers.default)(data: DataFile): Result[H] =
    UploadEx.hParser(parseH, data.file)

  /**
    * Attempt to parse the last line of the file into the trailer data type.
    *
    * The default trailer parser is described in [[parseHT]].
    * The user can supply their own parsing function and trailer data type if necesary.
    *
    * @tparam T Trailer data type
    *
    * @param parseT Function to parse trailer
    * @param file   Input source file
    *
    * @return Trailer data
    */
  def parseTrailer[T](parseT: String => Result[T] = TrailerParsers.default)(data: DataFile): Result[T] =
    UploadEx.tParser(parseT, data.file)

  /**
    * Attempts to parse the first and last line of the file into header and trailer data types.
    *
    * The default parser functions assume that the header and trailer lines look like:
    *
    * {{{
    * H|<BusinessDate>|<ExtractTime>|<FileName>
    * }}}
    * {{{
    * T|<RecordCount>|<CheckSumValue>|<CheckSumColumn>
    * }}}
    *
    * The user can supply their own parsing functions and types if required.
    *
    * @tparam H Header data type
    * @tparam T Trailer data type
    *
    * @param parseH Function that parses the header
    * @param parseT Function that parses the trailer
    *
    * @return Header and trailer data
    */
  def parseHT[H, T](parseH: String => Result[H] = HeaderParsers.default, parseT: String => Result[T] = TrailerParsers.default)(data: DataFile): Result[(H, T)] = {
    for {
      header   <- UploadEx.hParser(parseH, data.file)
      trailer  <- UploadEx.tParser(parseT, data.file)
    } yield(header, trailer)
  }

  /**
    * Find source files in the local ingestion path
    *
    * See the description of the files we upload in [[upload]].
    *
    * @param config The upload configuration: [[UploadConfig]].
    *
    * @return List of source files in the local ingestion path
    */
  def findSources(config: UploadConfig): Execution[List[DataFile]] =
    UploadEx.matcher(config)

  /**
    * Pushes a list of source files onto HDFS and archives them locally.
    *
    * See the description of the HDFS push and archive at [[upload]].
    *
    * @param config Upload configuration: [[UploadConfig]]
    * @param files  List of source files to upload
    *
    * @return List of source file destinations on HDFS
    */
  def uploadSources(config: UploadConfig, files: List[DataFile]): Execution[UploadInfo] =
    UploadEx.uploader(config, files)
}

/**
  * WARNING: not considered part of the load execution api.
  * We may change this without considering backwards compatibility.
  */
object UploadEx {
  val logger = LoggerFactory.getLogger("Upload")

  def matcher(conf: UploadConfig): Execution[List[DataFile]] = for {
    _     <- Execution.from {
               logger.info(s"Start of pattern matching from ${conf.localIngestPath}")
               logger.info(conf.prettyPrint)
             }
    files <- Execution.fromEither(Input.findFiles(
               conf.localIngestPath, conf.tablename, conf.filePattern, conf.controlPattern
             ))
    _     <- Execution.from(
               files.controlFiles.foreach(ctrl => logger.info(s"skipping control file ${ctrl.file.getName}"))
             )
  } yield files.dataFiles

  def uploader(conf: UploadConfig, files: List[DataFile]): Execution[UploadInfo] = for {
    _      <- Execution.from(logger.info("Starting fileUploads"))
    copied <- Execution.fromHdfs(
                Push.push(files, conf.hdfsLandingPath, conf.localArchivePath, conf.hdfsArchivePath)
              )
    _      <- Execution.from {
                copied.foreach { case Copied(files, dest) => logger.info(s"Copied ${files.map(_.getName).mkString(", ")} to $dest ")}
                logger.info(s"Upload ended")
              }
  } yield UploadInfo(copied.map(_.dest.toString))

  def hParser[H](parseH: String => Result[H], file: String): Result[H] =
    Result.safe {
      val reader = new BufferedReader(new FileReader(file))
      try     { reader.readLine }
      finally { reader.close }
    }.flatMap(parseH(_))

  def tParser[T](parseT: String => Result[T], file: String): Result[T] =
    Result.safe {
      val reader = new ReversedLinesFileReader(new File(file))
      try     {
        var line = reader.readLine
        while (line.isEmpty || line.forall(_.isWhitespace)) { line = reader.readLine }
        line
      }
      finally { reader.close }
    }.flatMap(parseT(_))
}
