// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
package com.digitalasset.codegen

import java.io.File

import com.digitalasset.codegen.lf.EnvironmentInterface
import com.digitalasset.daml.lf.iface.PackageId
import com.digitalasset.daml.lf.iface.reader.Errors.ErrorLoc
import com.digitalasset.daml.lf.iface.reader.{Errors, Interface, InterfaceReader}
import com.digitalasset.daml.lf.{Dar, UniversalArchiveReader}
import com.digitalasset.daml_lf.DamlLf
import scalaz.\/
import scalaz.Cord
import scalaz.syntax.bind._
import scalaz.syntax.traverse1._

import scala.util.{Failure, Success}

object ScalaCodeGen {

  type Payload = (PackageId, DamlLf.ArchivePayload)

  def generateScalaCode(files: List[File], packageName: String, outputDir: File): Unit = {

    val reader = UniversalArchiveReader()

  }

  private def generateScalaCode(parse: File => String \/ Dar[Payload])(f: File): String \/ Unit = {
    for {
      dar <- parse(f)
      _ <- decodeInterface(dar)

    } yield ???

  }

  private def parsePayload(reader: UniversalArchiveReader[Payload])(
      f: File): String \/ Dar[Payload] =
    reader.readArchive(f) match {
      case Success(p) => \/.right(p)
      case Failure(e) =>
        e.printStackTrace()
        \/.left(e.getLocalizedMessage)
    }

  private def decodeInterface(dar: Dar[Payload]): String \/ EnvironmentInterface = {
    import scalaz.syntax.traverse._
    dar.traverseU(decodeInterface).map(combineInterfaces)
  }

  private def decodeInterface(p: Payload): String \/ Interface =
    \/.fromTryCatchNonFatal {
      printProgress(s"decoding archive with Package ID: ${p._1.underlyingString: String}")
      val (errors, out) = Interface.read(p)
      println(s"decoded archive with Package ID: ${out.packageId.underlyingString: String}")

      if (!errors.empty) {
        \/.left(formatErrors("Errors decoding LF archive:\n", errors))
      } else \/.right(out)
    }.leftMap(_.getLocalizedMessage).join

  private def formatErrors(
      prefix: String,
      errors: Errors[ErrorLoc, InterfaceReader.InvalidDataTypeDefinition]): String =
    (Cord(prefix) ++ InterfaceReader.InterfaceReaderError.treeReport(errors)).toString

  private def combineInterfaces(dar: Dar[Interface]): EnvironmentInterface =
    EnvironmentInterface.fromReaderInterfaces(dar.main, dar.dependencies: _*)

  private def printProgress(msg: String): Unit = println(s"Scala Codegen - $msg")
}
