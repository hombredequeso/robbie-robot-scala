package com.hombredequeso.robbierobot

import com.hombredequeso.geneticAlgorithm.Optimizer
import com.hombredequeso.robbierobot.Evolve.{GenerationContext, GenerationStatistics}
import com.hombredequeso.robbierobot.Strategy.StrategyMap
import com.hombredequeso.util.RND.ScalaRandomizer
import com.hombredequeso.util.State
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

      val stats = new GenerationStatistics(List(), 0)
      val generationContext = new GenerationContext(new ScalaRandomizer(), stats)

      val getFitness =
        (strategy: StrategyMap) =>
          Strategy.getStrategyFitness(randomizer)(boardCount, numberOfTurnsPerBoard)(strategy)._1
      val getNextPopulation = Evolve.generateNextPopulation(getFitness)_
      val getFitness2 = getFitness.andThen(i => State.unit[GenerationContext, Int](i))
      val resultOfGenerations =
        Optimizer.executeGenerations(getNextPopulation)(State.unit(initialPopulation))(iterationCount)
      val (finalPopulation, finalGenerationContext) = resultOfGenerations.run(generationContext)
      val bestStrategy: ((StrategyMap, Int), GenerationContext) =
        Optimizer.getBestStrategy(getFitness2)(finalPopulation).run(finalGenerationContext)

      val finalFitness = bestStrategy._1._2
      Console.println(s"Final fitness: ${finalFitness}")
      finalFitness should be >0

    }
  }
}

