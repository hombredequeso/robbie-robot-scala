package com.hombredequeso.robbierobot


import com.hombredequeso.util.RND.ScalaRandomizer
import org.scalatest.{FunSpec, Matchers}

class StrategyFactorySpec  extends FunSpec with Matchers {


  describe("createRandomAction") {
    it("returns a reasonable spread of actions") {
      val count = 100
      val randomizer = new ScalaRandomizer()

      val counts = StrategyFactory
        .createRandomActions(randomizer)
        .take(count)
        .groupBy(a => a.id)
        .map(x => x._2.length)
        .toList
      all (counts) should be > 5
      // A test that will probably succeed.
    }
  }

}
