package com.hombredequeso.robbierobot

import org.scalatest.{FunSpec, Matchers}


class GeneticAlgorithmSpec extends FunSpec with Matchers {

  describe("findOptimalStrategy") {
    var iterationCount = 3;

    it(s"does not blow up in a puff of smoke (iterationCount = ${iterationCount})") {
      GeneticAlgorithm.findOptimalStrategy(3)
    }
  }
}
