package com.hombredequeso.geneticAlgorithm

object Optimizer {

  def evolveOverGenerations[A]
  (generateNextPopulation: => Vector[A] => Vector[A])
  (generationCount: Int )
  (population: Vector[A])
  : Vector[A] = {
    (0 until generationCount).foldLeft(population)((p,_) => generateNextPopulation(p))
  }

  def findOptimalStrategyAndFitness[A]
  (generateNextPopulation: (A => Int) => Vector[A] => Vector[A])
  (getFitness: A => Int)
  (generationCount: Int)
  (initialPopulation: Vector[A])
  : (A,Int) = {
    val getNextPopulation = generateNextPopulation(getFitness)
    val endPopulation = evolveOverGenerations(getNextPopulation)(generationCount)(initialPopulation)
    val finalResultWithFitness = endPopulation.map(a => (a, getFitness(a)))
    val fitness = (x: (A,Int)) => x._2
    val bestStrategy: (A, Int) = finalResultWithFitness.sortBy(fitness).last
    bestStrategy
  }
}

