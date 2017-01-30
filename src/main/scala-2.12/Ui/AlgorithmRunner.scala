package com.hombredequeso.robbierobot.Ui

import com.hombredequeso.geneticAlgorithm.Optimizer
import com.hombredequeso.robbierobot.Evolve.{GenerationContext, GenerationStatistics}
import com.hombredequeso.robbierobot.Strategy.StrategyMap
import com.hombredequeso.robbierobot.{Evolve, Strategy, StrategyFactory}
import com.hombredequeso.util.RND.ScalaRandomizer

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

    // State management:
    var stats = new GenerationStatistics(List())
    var randomizerState: GenerationContext =
      new GenerationContext(new ScalaRandomizer(), stats)

    def f2(getFitness: StrategyMap => Int)(population: Vector[StrategyMap]) = {
      val result = Evolve.generateNextPopulation(getFitness)(population).run(randomizerState)
      result._1
    }

    val result = Optimizer.findOptimalStrategyAndFitness(f2)(getFitness)(iterationCount)(initialPopulation)
    result
  }
}

