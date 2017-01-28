package com.hombredequeso.robbierobot


import com.hombredequeso.util.RND
import com.hombredequeso.util.RND.ScalaRandomizer
import org.scalatest.{FunSpec, Matchers}

class RndSpec extends FunSpec with Matchers {

  describe("nextIntsB") {
    it("return a list of random integers of the correct count") {
      val count = 10
      val (intList, nextRandomizer) =
        RND.nextN(count)(r => r.nextInt)(new ScalaRandomizer())

      intList.length shouldBe count
    }

    it("return a list of different numbers (in all likelihood)") {
      val count = 10
      val (intList, nextRandomizer) =
        RND.nextN(count)(r => r.nextInt)(new ScalaRandomizer())

      intList.map(i => (i -> 0)).toMap.size shouldBe count
    }
  }
}
