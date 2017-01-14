package com.hombredequeso.robbierobot

import com.hombredequeso.robbierobot.Evolve._
import com.hombredequeso.robbierobot.Strat.Strategy

object StrategyAlgorithm {
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
}

object GeneticAlgorithm {

  def generateNextPopulation
  (getFitness: Strategy => Int)
  (population: Vector[Strategy])
  : Vector[Strategy] = {
    val members: Vector[Member] =
      population.map(
        s => Member(s, getFitness(s)))
    val newPopulation = Evolve.evolve(members)
    newPopulation
  }

  def evolveOverGenerations
  (getFitness: Strategy => Int)
  (population: Vector[Strategy], generationCount: Int )
  : Vector[Strategy] = {
    if (generationCount == 0)
      population
    else {
      val newStrategies = generateNextPopulation(getFitness)(population)
      evolveOverGenerations(getFitness)(newStrategies, generationCount - 1)
    }
  }

  def findOptimalStrategy
  // (breedNextGeneration: Vector[Strategy] => Vector[Strategy])
  (getFitness: Strategy => Int)
  (generationCount: Int, populationSize: Int)
  (boardCount: Int, numberOfTurnsPerBoard: Int)
  (initialPopulation: Vector[Strategy])
  : Strategy = {

    val finalResult: Vector[Strategy] =
      evolveOverGenerations(getFitness)(initialPopulation, generationCount)
    val finalResultWithFitness =
      finalResult
        .map(s => Member(s, getFitness(s)))
    val bestStrategy = finalResultWithFitness.sortBy(x => x.fitness).last
    bestStrategy.strategy
  }
}
