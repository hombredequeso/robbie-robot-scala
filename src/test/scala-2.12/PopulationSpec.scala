package com.hombredequeso.robbierobot


import com.hombredequeso.robbierobot.Evolve.Member
import org.scalatest.{FunSpec, Matchers}

class PopulationSpec extends FunSpec with Matchers {
  describe("evolve"){
    it("returns a new population the same size as the original") {
      val strategies = StrategyFactory.make(
        StrategyFactory.allScenarios,
        StrategyFactory.createRandomActions())
      var member = Member(strategies, 1)
      var members = Vector(member)
      val result = Evolve.evolve(members)
      result.length shouldBe 1
    }
  }

  describe("evolveNewMember") {
    var iterationCount = 100;
    it(s"returns a complete strategy when provided a complete strategy: iterationCount = ${iterationCount}") {
      val members = (1 to iterationCount).map(_ =>
        StrategyFactory.make(
          StrategyFactory.allScenarios,
          StrategyFactory.createRandomActions())
      ).map(x => Member(x, 1))
      val result = Evolve.evolveNewMember(members.toVector)

      result.size shouldBe StrategyFactory.allScenarios.size
    }
  }
}
