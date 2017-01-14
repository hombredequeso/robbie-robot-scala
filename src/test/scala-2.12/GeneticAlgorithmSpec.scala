package com.hombredequeso.robbierobot

import com.hombredequeso.geneticAlgorithm.Optimizer
import org.scalatest.{FunSpec, Matchers}

class GeneticAlgorithmSpec extends FunSpec with Matchers {

  describe("findOptimalStrategy") {

    val iterationCount = 20;
    val populationSize = 100;
    val boardCount = 10;
    val numberOfTurnsPerBoard = 50;

    it(s"can execute ${iterationCount} generations") {
      val initialPopulation = StrategyFactory.createInitialPopulation(populationSize)
      val getFitness = Strategy.getStrategyFitness(boardCount, numberOfTurnsPerBoard)_
      Optimizer.findOptimalStrategy(Evolve.generateNextPopulation)(getFitness)(iterationCount)(initialPopulation)
    }
  }
}

