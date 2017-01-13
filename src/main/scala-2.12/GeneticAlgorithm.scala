package com.hombredequeso.robbierobot

import com.hombredequeso.robbierobot.Evolve._
import com.hombredequeso.robbierobot.Strat.Strategy

object GeneticAlgorithm {


  def findOptimalStrategy(iterations: Int, populationSize: Int): Strategy = {
    val initialPopulation = createInitialPopulation(populationSize)

    def getStrategyFitness(strategy: Strategy): Int = {
      val boardCount = 100
      val numberOfTurnsPerBoard = 200
      val boards = (1 to boardCount).map(x => Board.createRandomBoard(100, 100, 0.5f))
      val fitness = boards.map(b =>
        Play.execute(
          Play.State(b, Play.initialRobotPosition),
          strategy,
          numberOfTurnsPerBoard))
      fitness.sum
    }

    def iterate(population: Vector[Strategy]): Vector[Strategy] = {
      val members = population.map(s => Member(s, getStrategyFitness(s)))
      val newPopulation = Evolve.evolve(members)
      newPopulation
    }


    def goForIt(population: Vector[Strategy], iterationsRemaining: Int ): Vector[Strategy] = {
      if (iterationsRemaining == 0)
        population
      else {
        val newStrategies = iterate(population)
        goForIt(newStrategies, iterationsRemaining - 1)
      }
    }

    val finalResult = goForIt(initialPopulation, iterations)
    val finalResultWithFitness = finalResult.map(s => Member(s, getStrategyFitness(s)))
    val bestStrategy = finalResultWithFitness.sortBy(x => x.fitness).last
    bestStrategy.strategy
  }
}
