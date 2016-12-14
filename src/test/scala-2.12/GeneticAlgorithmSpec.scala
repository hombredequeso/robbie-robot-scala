package com.hombredequeso.robbierobot

import org.scalatest.{FunSpec, Matchers}


class GeneticAlgorithmSpec extends FunSpec with Matchers {

  describe("findOptimalStrategy") {
    it("does not blow up in a puff of smoke") {
      val result = GeneticAlgorithm.findOptimalStrategy()
    }
  }
}
