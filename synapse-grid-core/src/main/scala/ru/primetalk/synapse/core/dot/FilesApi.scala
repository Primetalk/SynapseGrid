package ru.primetalk.synapse.core.dot

import java.io.{File, PrintWriter}

import scala.language.implicitConversions
import scala.util.Try

/**
 * Simple API for saving files
 * @author zhizhelev, 25.03.15.
 */
trait FilesApi {

  implicit class WritableString(s: String) {
    def saveTo(filePath: String): Unit = {
      val wrt = new PrintWriter(new File(filePath), "UTF-8")
      try {
        wrt.print(s)
      } finally {
        wrt.close()
      }
    }
    def trySaveTo(filePath: String): Try[Unit] = Try{saveTo(filePath)}
  }

  implicit def filenameToFile(filename: String): File = new File(filename)

}
