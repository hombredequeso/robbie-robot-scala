package com.hombredequeso.robbierobot.Ui

import com.hombredequeso.geneticAlgorithm.Optimizer
import com.hombredequeso.robbierobot.Evolve.{GenerationContext, GenerationStatistics}
import com.hombredequeso.robbierobot.Strategy.StrategyMap
import com.hombredequeso.robbierobot.{Evolve, Strategy, StrategyFactory}
import com.hombredequeso.util.RND.ScalaRandomizer
import com.hombredequeso.util.State

object AlgorithmRunner {
  val iterationCount = 200
  val populationSize = 200
  val boardCount = 100
  val numberOfTurnsPerBoard = 200

  def execute(): (StrategyMap,Int) ={
    val randomizer = new ScalaRandomizer()
    val initialPopulation = StrategyFactory.createInitialPopulation(randomizer)(populationSize)
    val getFitness =
      (strategy: StrategyMap) =>
        Strategy.getStrategyFitness(randomizer)(boardCount, numberOfTurnsPerBoard)(strategy)._1
    val getFitness2 = getFitness.andThen(i => State.unit[GenerationContext, Int](i))

    // State management:
    val stats = new GenerationStatistics(List(), 0)
    val randomizerState: GenerationContext =
      new GenerationContext(new ScalaRandomizer(), stats)


    val getNextPopulation = Evolve.generateNextPopulation(getFitness)_
    val resultOfGenerations =
      Optimizer.executeGenerations(getNextPopulation)(State.unit(initialPopulation))(iterationCount)
    val (finalPopulation, finalGenerationContext) = resultOfGenerations.run(randomizerState)
    val bestStrategy: ((StrategyMap, Int), GenerationContext) =
      Optimizer.getBestStrategy(getFitness2)(finalPopulation).run(finalGenerationContext)
    bestStrategy._1
  }
}

