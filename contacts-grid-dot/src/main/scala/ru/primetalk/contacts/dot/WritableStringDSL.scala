package ru.primetalk.contacts.dot

import java.io.{File, PrintWriter}

import scala.util.Try

/**
 * Simple API for saving files
 * @author zhizhelev, 25.03.15.
 */
object WritableStringDSL {

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

}
