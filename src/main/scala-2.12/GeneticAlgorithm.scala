package com.hombredequeso.robbierobot

import com.hombredequeso.robbierobot.Evolve._
import com.hombredequeso.robbierobot.Strat.Strategy

object GeneticAlgorithm {

  def evolveOverGenerations
  (generateNextPopulation: => Vector[Strategy] => Vector[Strategy])
  (generationCount: Int )
  (population: Vector[Strategy])
  : Vector[Strategy] = {
    (0 until generationCount).toList
      .foldLeft(population)((p,_) => generateNextPopulation(p))
  }

  def findOptimalStrategy
  (generateNextPopulation: (Strategy => Int) => Vector[Strategy] => Vector[Strategy])
  (getFitness: Strategy => Int)
  (generationCount: Int)
  (initialPopulation: Vector[Strategy])
  : Strategy = {
    val getNextPopulation: (Vector[Strategy]) => Vector[Strategy] =
      generateNextPopulation(getFitness)
    val endPopulation = evolveOverGenerations(getNextPopulation)(generationCount)(initialPopulation)
    val finalResultWithFitness = endPopulation.map(s => Member(s, getFitness(s)))
    val bestStrategy = finalResultWithFitness.sortBy(x => x.fitness).last
    bestStrategy.strategy
  }
}

