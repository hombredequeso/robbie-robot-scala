package com.hombredequeso.robbierobot.Ui

import com.hombredequeso.geneticAlgorithm.Optimizer
import com.hombredequeso.robbierobot.Evolve.GenerationContext
import com.hombredequeso.robbierobot.Strategy.StrategyMap
import com.hombredequeso.robbierobot.{Evolve, Strategy, StrategyFactory}
import com.hombredequeso.util.RND.ScalaRandomizer
import com.hombredequeso.util.State

object AlgorithmRunner {
  val iterationCount =  5
  val populationSize = 200
  val boardCount = 100
  val numberOfTurnsPerBoard = 200

  def execute(): State[GenerationContext, (StrategyMap,Int)] =  {
    val initialPopulation = StrategyFactory.createInitialPopulation(new ScalaRandomizer())(populationSize)

    val getFitness = (strategy: StrategyMap) =>
        Strategy.getStrategyFitness(new ScalaRandomizer())(boardCount, numberOfTurnsPerBoard)(strategy)._1
    val getFitness2 = getFitness.andThen(i => State.unit[GenerationContext, Int](i))
    val getNextPopulation = Evolve.generateNextPopulation(getFitness)_

    for {
      resultOfGenerations <- Optimizer.executeGenerations(
        getNextPopulation)(
        State.unit(initialPopulation))(
        iterationCount)
      bestStrategy <- Optimizer.getBestStrategy(getFitness2)(resultOfGenerations)
    } yield bestStrategy
  }
}

