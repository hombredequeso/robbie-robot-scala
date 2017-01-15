package com.hombredequeso.robbierobot

import com.hombredequeso.geneticAlgorithm.Optimizer
import org.scalatest.{FunSpec, Matchers}

class GeneticAlgorithmSpec extends FunSpec with Matchers {

  describe("findOptimalStrategy") {

    val iterationCount = 50
    val populationSize = 200
    val boardCount = 50 // 100
    val numberOfTurnsPerBoard = 100 // 200

    it(s"can execute ${iterationCount} generations") {
      val initialPopulation = StrategyFactory.createInitialPopulation(populationSize)
      val getFitness = Strategy.getStrategyFitness(boardCount, numberOfTurnsPerBoard)_
      Optimizer.findOptimalStrategy(Evolve.generateNextPopulation)(getFitness)(iterationCount)(initialPopulation)
    }
  }
}

