package com.hombredequeso.robbierobot

import com.hombredequeso.geneticAlgorithm.Optimizer
import com.hombredequeso.util.RND.ScalaRandomizer
import org.scalatest.{FunSpec, Matchers}

class GeneticAlgorithmSpec extends FunSpec with Matchers {

  describe("findOptimalStrategy") {

    val iterationCount = 10
    val populationSize = 200
    val boardCount = 100
    val numberOfTurnsPerBoard = 200

    it(s"can get fitness over 0 with ${iterationCount} generations") {
      val randomizer = new ScalaRandomizer()
      val initialPopulation = StrategyFactory.createInitialPopulation(randomizer)(populationSize)
      val getFitness = Strategy.getStrategyFitness(randomizer)(boardCount, numberOfTurnsPerBoard)_
      val result = Optimizer.findOptimalStrategyAndFitness(
        Evolve.generateNextPopulation(
          new ScalaRandomizer())(
          StatWriter.write))(
        a => getFitness(a)._1)(
        iterationCount)(
        initialPopulation)

      Console.println(s"Final fitness: ${result._2}")
      result._2 should be >0
    }
  }
}

