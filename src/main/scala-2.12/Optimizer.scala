package com.hombredequeso.geneticAlgorithm

object Optimizer {

  def evolveOverGenerations[A]
  (generateNextPopulation: => Vector[A] => Vector[A])
  (generationCount: Int )
  (population: Vector[A])
  : Vector[A] = {
    (0 until generationCount).toList
      .foldLeft(population)((p,_) => generateNextPopulation(p))
  }

  def findOptimalStrategy[A]
  (generateNextPopulation: (A => Int) => Vector[A] => Vector[A])
  (getFitness: A => Int)
  (generationCount: Int)
  (initialPopulation: Vector[A])
  : A = {
    val getNextPopulation = generateNextPopulation(getFitness)
    val endPopulation = evolveOverGenerations(getNextPopulation)(generationCount)(initialPopulation)
    val finalResultWithFitness = endPopulation.map(a => (a, getFitness(a)))
    val fitness = (x: (A,Int)) => x._2
    val bestStrategy = finalResultWithFitness.sortBy(fitness).last
    Console.println(s"Final fitness: ${bestStrategy._2}")
    bestStrategy._1
  }
}

