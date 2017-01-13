package com.hombredequeso.robbierobot

import com.hombredequeso.robbierobot.Evolve._
import com.hombredequeso.robbierobot.Strat.Strategy

object GeneticAlgorithm {

  def getStrategyFitness
  (boardCount: Int, numberOfTurnsPerBoard: Int)
  (strategy: Strategy)
  : Int = {
    val boards = (1 to boardCount).map(x => Board.createRandomBoard(100, 100, 0.5f))
    val fitness = boards.map(b =>
      Play.execute(
        Play.State(b, Play.initialRobotPosition),
        strategy,
        numberOfTurnsPerBoard))
    fitness.sum
  }

  def generateNextPopulation
  (boardCount: Int, numberOfTurnsPerBoard: Int)
  (population: Vector[Strategy])
  : Vector[Strategy] = {
    val members: Vector[Member] =
      population.map(
        s => Member(s, getStrategyFitness(boardCount, numberOfTurnsPerBoard)(s)))
    val newPopulation = Evolve.evolve(members)
    newPopulation
  }

  def evolveOverGenerations
  (population: Vector[Strategy], generationCount: Int )
  (boardCount: Int, numberOfTurnsPerBoard: Int)
  : Vector[Strategy] = {
    if (generationCount == 0)
      population
    else {
      val newStrategies = generateNextPopulation(
        boardCount, numberOfTurnsPerBoard)(
        population)
      evolveOverGenerations(newStrategies, generationCount - 1)(boardCount, numberOfTurnsPerBoard)
    }
  }

  def findOptimalStrategy
  (generationCount: Int, populationSize: Int)
  (boardCount: Int, numberOfTurnsPerBoard: Int)
  (initialPopulation: Vector[Strategy])
  : Strategy = {

    val finalResult: Vector[Strategy] =
      evolveOverGenerations(initialPopulation, generationCount)(boardCount, numberOfTurnsPerBoard)
    val finalResultWithFitness =
      finalResult
        .map(s => Member(s, getStrategyFitness(boardCount, numberOfTurnsPerBoard)(s)))
    val bestStrategy = finalResultWithFitness.sortBy(x => x.fitness).last
    bestStrategy.strategy
  }
}
