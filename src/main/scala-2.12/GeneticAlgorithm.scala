package com.hombredequeso.robbierobot

import com.hombredequeso.robbierobot.Evolve._
import com.hombredequeso.robbierobot.Strat.Strategy

object GeneticAlgorithm {

  def getStrategyFitness
  (strategy: Strategy)
  (boardCount: Int, numberOfTurnsPerBoard: Int)
  : Int = {
    val boards = (1 to boardCount).map(x => Board.createRandomBoard(100, 100, 0.5f))
    val fitness = boards.map(b =>
      Play.execute(
        Play.State(b, Play.initialRobotPosition),
        strategy,
        numberOfTurnsPerBoard))
    fitness.sum
  }

  def iterate
  (population: Vector[Strategy])
  (boardCount: Int, numberOfTurnsPerBoard: Int)
  : Vector[Strategy] = {
    val members =
      population.map(
        s => Member(s, getStrategyFitness(s)(boardCount, numberOfTurnsPerBoard)))
    val newPopulation = Evolve.evolve(members)
    newPopulation
  }


  def goForIt
  (population: Vector[Strategy], iterationsRemaining: Int )
  (boardCount: Int, numberOfTurnsPerBoard: Int)
  : Vector[Strategy] = {
    if (iterationsRemaining == 0)
      population
    else {
      val newStrategies = iterate(population)(boardCount, numberOfTurnsPerBoard)
      goForIt(newStrategies, iterationsRemaining - 1)(boardCount, numberOfTurnsPerBoard)
    }
  }

  def findOptimalStrategy
  (iterations: Int, populationSize: Int)
  (boardCount: Int, numberOfTurnsPerBoard: Int)
  : Strategy = {
    val initialPopulation = createInitialPopulation(populationSize)

    val finalResult =
      goForIt(initialPopulation, iterations)(boardCount, numberOfTurnsPerBoard)
    val finalResultWithFitness =
      finalResult
        .map(s => Member(s, getStrategyFitness(s)(boardCount, numberOfTurnsPerBoard)))
    val bestStrategy = finalResultWithFitness.sortBy(x => x.fitness).last
    bestStrategy.strategy
  }
}
