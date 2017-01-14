package com.hombredequeso.robbierobot


import com.hombredequeso.robbierobot.Evolve.Member
import org.scalatest.{FunSpec, Matchers}

class PopulationSpec extends FunSpec with Matchers {

  describe("getRandomWeighted") {
    it("with one element returns the one element") {
      val x = Vector((0, "test"))
      val result = Evolve.getWeightedRandom(x)
      result shouldBe "test"
    }

    it("with two equally weighted elements returns each about 50% of the time") {
      val x = Vector((0, -1), (0, 1))
      val result = (0 to 100).map(_ => Evolve.getWeightedRandom(x)).sum
      val beWithinTolerance = be >= -20 and be <= 20
      result should beWithinTolerance
    }
  }

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
