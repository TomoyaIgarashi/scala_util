package jp.co.couger.util

import javax.crypto.{Cipher, Mac}
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

import org.apache.commons.codec.binary.Base64

import scala.util.Random
import scala.util.control.Exception._

import scalaz.Scalaz._

/**
 * Created by tomoya@couger.co.jp on 3/18/15.
 */
object Crypt {

  /**
   * Return compressed encrypted String
   * @param source Source String
   * @param secretKey Secret Key
   * @param algoCipher Specify
   * @param paddingCipher
   * @param algoMAC
   * @param charsetName
   * @return
   */
  def encrypt(
    source: String,
    secretKey: String = "",
    algoCipher: String = "AES",
    paddingCipher: String = "AES/CBC/PKCS5Padding",
    algoMAC: String = "HmacSHA1",
    charsetName: String = "UTF-8"): Either[Throwable, String] = {

    require(Option(source).nonEmpty)

    def randomByte(n: Int) = {
      val r = Random.shuffle((0 until 255).toList).headOption | 0
      r.toByte
    }
    val iv = (0 until 16).toArray map { randomByte }
    val base64Iv = new String(Base64.encodeBase64(iv))

    def encrypt(
      source: Array[Byte],
      cipher: Cipher,
      base64Iv: String): Either[Throwable, String] = {

      allCatch either {
        val e = cipher.doFinal(source)
        val base64Enc = new String(Base64.encodeBase64(e))
        val bs = (base64Enc + "--" + base64Iv).getBytes
        new String(Base64.encodeBase64(bs))
      }
    }

    for {
      r1 <- GZIP.compress(source, charsetName)
      r2 <- cipher(Cipher.ENCRYPT_MODE, secretKey, algoCipher, paddingCipher, iv)
      r3 <- encrypt(r1, r2, base64Iv)
      r4 <- mac(r3, secretKey, algoMAC)
    } yield {
      r3 + "--" + r4
    }
  }

  /**
   * Return decrypted String
   * @param digest
   * @param secretKey
   * @param algoCipher
   * @param paddingCipher
   * @param algoMAC
   * @param charsetName
   * @return
   */
  def decrypt(
    digest: String,
    secretKey: String,
    algoCipher: String = "AES",
    paddingCipher: String = "AES/CBC/PKCS5Padding",
    algoMAC: String = "HmacSHA1",
    charsetName: String = "UTF-8"): Either[Throwable, String] = {

    require(Option(digest).nonEmpty)

    def split(digest: String): Either[Throwable, Seq[Array[Byte]]] = {
      allCatch either {
        val split1 = digest.split("--")
        val encrypted = split1(0)
        val encAndIv = new String(Base64.decodeBase64(encrypted.getBytes))
        val split2 = encAndIv.split("--")
        val encoded = Base64.decodeBase64(split2(0).getBytes)
        val iv = Base64.decodeBase64(split2(1).getBytes)
        Seq(encoded, iv)
      }
    }

    def decrypt(encoded: Array[Byte], cipher: Cipher): Either[Throwable, Array[Byte]] = {
      allCatch either {
        cipher.doFinal(encoded)
      }
    }

    for {
      r1 <- split(digest)
      r2 <- cipher(Cipher.DECRYPT_MODE, secretKey, algoCipher, paddingCipher, r1(1))
      r3 <- decrypt(r1(0), r2)
      r4 <- GZIP.decompress(r3, charsetName)
    } yield {
      r4
    }
  }

  private def cipher(
    mode: Int,
    secretKey: String,
    algorithm: String,
    padding: String,
    iv: Array[Byte]): Either[Throwable, Cipher] = {

    allCatch either {
      val c = Cipher.getInstance(padding)
      val sks = new SecretKeySpec(secretKey.getBytes, algorithm)
      val ips = new IvParameterSpec(iv)
      c.init(mode, sks, ips)
      c
    }
  }

  private def mac(
    message: String,
    secretKey: String,
    algorithm: String): Either[Throwable, String] = {

    allCatch either {
      val sks = new SecretKeySpec(secretKey.getBytes, algorithm)
      val mac = Mac.getInstance(algorithm)
      mac.init(sks)
      val digest = mac.doFinal(message.getBytes)
      val hex = digest map { b => "%02x".format(b) }
      hex.mkString
    }
  }

}
