package com.hombredequeso.robbierobot

import org.scalatest.{FunSpec, Matchers}


class GeneticAlgorithmSpec extends FunSpec with Matchers {

  describe("findOptimalStrategy") {
    var iterationCount = 3;
    val populationSize = 100;
    var boardCount = 10;
    var numberOfTurnsPerBoard = 50;

    it(s"does not blow up in a puff of smoke (iterationCount = ${iterationCount})") {
      val initialPopulation = StrategyFactory.createInitialPopulation(populationSize)
      val getFitness = StrategyAlgorithm.getStrategyFitness(boardCount, numberOfTurnsPerBoard)_

      GeneticAlgorithm.findOptimalStrategy(
        getFitness)(
        iterationCount, populationSize)(
        boardCount, numberOfTurnsPerBoard)(
        initialPopulation)
    }
  }
}
