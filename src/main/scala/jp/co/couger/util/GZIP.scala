package jp.co.couger.util

import java.io.{BufferedReader, ByteArrayInputStream, ByteArrayOutputStream, InputStreamReader}
import java.nio.charset.Charset
import java.util.zip._

import scala.util.control.Exception._

import scalaz.Scalaz._

/**
 * Created by tomoya@couger.co.jp on 3/17/15.
 */
object GZIP {

  /**
   * Return gzip compressed byte array
   * @param source Source String
   * @param charsetName
   * @return Either[Throwable, Array[Byte]]
   */
  def compress(source: String, charsetName: String): Either[Throwable, Array[Byte]] = {
    val buffer = new ByteArrayOutputStream()
    def compressString(source: String, charset: Charset): Either[Throwable, Unit] = {
      val gzos = new GZIPOutputStream(buffer)
      allCatch andFinally {
        gzos.close()
      } either {
        gzos.write(source.getBytes(charset))
      }
    }
    def toByteArray: Either[Throwable, Array[Byte]] = {
      allCatch andFinally {
        buffer.close()
      } either {
        buffer.toByteArray()
      }
    }
    for {
      r1 <- getCharset(charsetName)
      r2 <- compressString(source, r1)
      r3 <- toByteArray
    } yield r3
  }

  /**
   * Return decompressed string
   * @param data
   * @param charsetName
   * @return Either[Throwable, String]
   */
  def decompress(data: Array[Byte], charsetName: String): Either[Throwable, String] = {
    def decompressByteArray(data: Array[Byte], charset: Charset): Either[Throwable, String] = {
      val bais = new ByteArrayInputStream(data)
      val gzis = new GZIPInputStream(bais)
      val reader = new InputStreamReader(gzis)
      val in = new BufferedReader(reader)
      val sb = new StringBuilder
      allCatch andFinally {
        in.close()
        reader.close()
        gzis.close()
        bais.close()
      } either {
        Iterator continually {
          in.readLine()
        } takeWhile {
          _ != null
        } foreach {
          sb.append(_)
        }
        sb.result
      }
    }
    for {
      r1 <- getCharset(charsetName)
      r2 <- decompressByteArray(data, r1)
    } yield r2
  }

  private def getCharset(charsetName: String) =
    allCatch either { Charset.forName(charsetName) }
}
