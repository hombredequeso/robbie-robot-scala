package com.hombredequeso.robbierobot.Ui

import com.hombredequeso.geneticAlgorithm.Optimizer
import com.hombredequeso.robbierobot.Strategy.StrategyMap
import com.hombredequeso.robbierobot.{Evolve, Strategy, StrategyFactory}

object AlgorithmRunner {
  val iterationCount = 5
  val populationSize = 200
  val boardCount = 100
  val numberOfTurnsPerBoard = 200

  def execute(): StrategyMap ={
    val initialPopulation = StrategyFactory.createInitialPopulation(populationSize)
    val getFitness = Strategy.getStrategyFitness(boardCount, numberOfTurnsPerBoard)_
    val result: StrategyMap = Optimizer.findOptimalStrategy(Evolve.generateNextPopulation)(getFitness)(iterationCount)(initialPopulation)
    result
  }
}

