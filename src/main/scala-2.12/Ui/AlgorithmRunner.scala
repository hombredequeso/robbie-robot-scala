package com.hombredequeso.robbierobot.Ui

import com.hombredequeso.geneticAlgorithm.Optimizer
import com.hombredequeso.robbierobot.Strategy.StrategyMap
import com.hombredequeso.robbierobot.{Evolve, Strategy, StrategyFactory}
import com.hombredequeso.util.RND.ScalaRandomizer

object AlgorithmRunner {
  val iterationCount = 5
  val populationSize = 200
  val boardCount = 100
  val numberOfTurnsPerBoard = 200

  def execute(): StrategyMap ={
    val initialPopulation = StrategyFactory.createInitialPopulation(populationSize)
    val randomizer = new ScalaRandomizer()
    val getFitness = Strategy.getStrategyFitness(randomizer)(boardCount, numberOfTurnsPerBoard)_
    val result: StrategyMap = Optimizer.findOptimalStrategy(
      Evolve.generateNextPopulation(randomizer))(getFitness)(iterationCount)(initialPopulation)
    result
  }
}

