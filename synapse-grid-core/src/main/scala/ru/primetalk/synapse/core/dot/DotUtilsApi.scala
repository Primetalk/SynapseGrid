package ru.primetalk.synapse.core.dot

import java.io.{File, PrintWriter}

import scala.language.implicitConversions

/**
 * Simple API for saving files
 * @author zhizhelev, 25.03.15.
 */
trait DotUtilsApi {

  implicit class WritableString(s: String) {
    def saveTo(filePath: String) {
      val wrt = new PrintWriter(new File(filePath), "UTF-8")
      try {
        wrt.print(s)
      } finally {
        wrt.close()
      }
    }
  }

  implicit def filenameToFile(filename: String): File = new File(filename)

}
