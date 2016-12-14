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
      val result = (0 to 10).map(_ => Evolve.getWeightedRandom(x)).sum
      val beWithinTolerance = be >= -2 and be <= 2
      result should beWithinTolerance
    }
  }

  describe("totalWeight") {
   it("with one 0 element returns 1") {
     val x = Vector((0, "test"))
     Evolve.getTotalWeight(x) shouldBe 1
   }

    it("with two 0 element returns 2") {
      val x = Vector((0, "test"), (0, "test"))
      Evolve.getTotalWeight(x) shouldBe 2
    }

    it("with one 10 element returns 1") {
      val x = Vector((0, "test"), (0, "test"))
      Evolve.getTotalWeight(x) shouldBe 2
    }

    it("with n of the same element returns n") {
      val n = 20
      val weight = 70
      val v = (1 to n).map(x => (weight,x)).toVector
      Evolve.getTotalWeight(v) shouldBe n
    }

    it("with two elements returns ??") {
      val x = Vector((0, "test"), (1, "test"))
      Evolve.getTotalWeight(x) shouldBe 3
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
    it("returns a complete strategy when provided a complete strategy") {
      val members = (1 to 100).map(_ =>
        StrategyFactory.make(
          StrategyFactory.allScenarios,
          StrategyFactory.createRandomActions())
      ).map(x => Member(x, 1))
      val result = Evolve.evolveNewMember(members.toVector)

      result.size shouldBe StrategyFactory.allScenarios.size
    }
  }
}
