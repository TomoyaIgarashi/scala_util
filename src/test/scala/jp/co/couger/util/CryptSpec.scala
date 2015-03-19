package jp.co.couger.util

import org.specs2.mutable.Specification

/**
 * Created by tomoya@couger.co.jp on 3/18/15.
 */
class CryptSpec extends Specification {

  "Crypt" should {
    "Negative Testing" in {
      "encrypt" in {
        "NullPointerException when source is null" in {
          val ee = Crypt.encrypt(null, "abcdefghijklmnopqrstuvwxyz012345")
          ee must beLeft.like {
            case e: NullPointerException => e.getMessage must beNull
          }
        }
        "IllegalArgumentException(Empty key) when secretKey is None" in {
          val ee = Crypt.encrypt("abc", "")
          ee must beLeft.like {
            case e: IllegalArgumentException => e.getMessage must_== "Empty key"
          }
        }
        "NullPointerException when secretKey is null" in {
          val ee = Crypt.encrypt("abc", null)
          ee must beLeft.like {
            case e: NullPointerException => e.getMessage must beNull
          }
        }
        "IllegalArgumentException(Invalid AES key length: 3 bytes) when secretKey is too short" in {
          val ee = Crypt.encrypt("abc", "abc")
          ee must beLeft.like {
            case e: java.security.InvalidKeyException => e.getMessage must_== "Invalid AES key length: 3 bytes"
          }
        }
      }

      "decrypt" in {
        "NullPointerException when source is null" in {
          val ed = Crypt.decrypt(null, "abcdefghijklmnopqrstuvwxyz012345")
          ed must beLeft.like {
            case e: NullPointerException => e.getMessage must beNull
          }
        }
        "ArrayIndexOutOfBoundsException(1) when source is null" in {
          val ed = Crypt.decrypt("", "abcdefghijklmnopqrstuvwxyz012345")
          ed must beLeft.like {
            case e: ArrayIndexOutOfBoundsException => e.getMessage must_== "1"
          }
        }
      }
    }

    "Positive Testing" should {
      "encrypt -> decrypt" in {
        val secretKey = "abcdefghijklmnopqrstuvwxyz012345"
        val ee = Crypt.encrypt("あいうえお", secretKey)
        val e = ee.fold(x => "", identity)
        val ed = Crypt.decrypt(e, secretKey)
        val d = ed.fold(x => "", identity)
        d must_== "あいうえお"
      }
    }
  }
}
