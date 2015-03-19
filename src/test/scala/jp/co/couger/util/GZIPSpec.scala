package jp.co.couger.util

import org.specs2.mutable.Specification

/**
 * Created by tomoya@couger.co.jp on 3/17/15.
 */
class GZIPSpec extends Specification {

  "GZIP" should {
    "compress -> decompress" in {
      val r1 = GZIP.compress("あいうえお", "UTF-8")
      val c = r1.fold(x => Array[Byte](0), identity)
      val r2 = GZIP.decompress(c, "UTF-8")
      val d = r2.fold(x => "", identity)
      d must_== "あいうえお"
    }
  }
}
