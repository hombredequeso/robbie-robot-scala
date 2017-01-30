package com.hombredequeso.robbierobot

import com.hombredequeso.geneticAlgorithm.Optimizer
import com.hombredequeso.robbierobot.Evolve.{GenerationContext, GenerationStatistics, Member}
import com.hombredequeso.robbierobot.Strategy.StrategyMap
import com.hombredequeso.util.RND.ScalaRandomizer
import com.hombredequeso.util.{RandomProvider, State}
import org.scalatest.{FunSpec, Matchers}

class GeneticAlgorithmSpec extends FunSpec with Matchers {

  describe("findOptimalStrategy") {

    val iterationCount = 2
    val populationSize = 100
    val boardCount = 20
    val numberOfTurnsPerBoard = 100

    it(s"can get fitness over 0 with ${iterationCount} generations") {
      val randomizer = new ScalaRandomizer()
      val initialPopulation = StrategyFactory.createInitialPopulation(randomizer)(populationSize)
      val getFitness = Strategy.getStrategyFitness(randomizer)(boardCount, numberOfTurnsPerBoard)_

      var stats = new GenerationStatistics(List())
      var randomizerState: GenerationContext =
        new GenerationContext(new ScalaRandomizer(), stats)

      def f2(getFitness: StrategyMap => Int)(population: Vector[StrategyMap]) = {
        val result = Evolve.generateNextPopulation(getFitness)(population).run(randomizerState)
        result._1
      }

      val result = Optimizer.findOptimalStrategyAndFitness(
        f2)(
        a => getFitness(a)._1)(
        iterationCount)(
        initialPopulation)

      Console.println(s"Final fitness: ${result._2}")
      result._2 should be >0
    }
  }
}

