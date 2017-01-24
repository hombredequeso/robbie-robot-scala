package com.hombredequeso.robbierobot.Ui

import com.hombredequeso.geneticAlgorithm.Optimizer
import com.hombredequeso.robbierobot.Strategy.StrategyMap
import com.hombredequeso.robbierobot.{Evolve, StatWriter, Strategy, StrategyFactory}
import com.hombredequeso.util.RND.ScalaRandomizer

object AlgorithmRunner {
  val iterationCount = 200
  val populationSize = 200
  val boardCount = 100
  val numberOfTurnsPerBoard = 200

  var iteration = 0

  def execute(): (StrategyMap,Int) ={
    val randomizer = new ScalaRandomizer()
    val initialPopulation = StrategyFactory.createInitialPopulation(randomizer)(populationSize)
    val getFitness = Strategy.getStrategyFitness(randomizer)(boardCount, numberOfTurnsPerBoard)_
    val result = Optimizer.findOptimalStrategyAndFitness(
      Evolve.generateNextPopulation(randomizer)(StatWriter.write))(getFitness)(iterationCount)(initialPopulation)
    result
  }
}

