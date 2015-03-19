package jp.co.couger.crypt

import org.specs2.mutable.Specification

import java.lang.Class._

/**
 * Created by tomoya@couger.co.jp on 3/18/15.
 */
class CryptSpec extends Specification {

  "Crypt" should {
    "encrypt -> decrypt" in {
      val secretKey = "abcdefghijklmnopqrstuvwxyz012345"
      val ee = Crypt.encrypt("abc", secretKey)
      val e = ee.fold(x => "", identity)
      val ed = Crypt.decrypt(e, secretKey)
      val d = ed.fold(x => "", identity)
      d must_== "abc"
    }
  }
}
