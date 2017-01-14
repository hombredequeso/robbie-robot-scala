package com.hombredequeso.robbierobot

import org.scalatest.{FunSpec, Matchers}

class GeneticAlgorithmSpec extends FunSpec with Matchers {

  describe("findOptimalStrategy") {

    val iterationCount = 3;
    val populationSize = 100;
    val boardCount = 10;
    val numberOfTurnsPerBoard = 50;

    it(s"can execute ${iterationCount} generations") {
      val initialPopulation = StrategyFactory.createInitialPopulation(populationSize)
      val getFitness = Strat.getStrategyFitness(boardCount, numberOfTurnsPerBoard)_
      GeneticAlgorithm.findOptimalStrategy(Evolve.generateNextPopulation)(getFitness)(iterationCount)(initialPopulation)
    }
  }
}
