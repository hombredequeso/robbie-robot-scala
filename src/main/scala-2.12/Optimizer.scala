package com.hombredequeso.geneticAlgorithm

import com.hombredequeso.util.State


object Optimizer {

  def evolveOverGenerations[A]
  (generateNextPopulation: => Vector[A] => Vector[A])
  (generationCount: Int )
  (population: Vector[A])
  : Vector[A] = {
    (0 until generationCount).foldLeft(population)((p,_) => generateNextPopulation(p))
  }

  def evolveOverGenerationsStateful[S, A]
  (generateNextPopulation: => Vector[A] => State[S, Vector[A]])
  (generationCount: Int )
  (population: State[S, Vector[A]])
  : State[S, Vector[A]] = {
    (0 until generationCount)
      .foldLeft(population)((p,_) => {
        p.flatMap(generateNextPopulation(_))
      })
  }

  def executeGenerations[S, A]
  (generateNextPopulation: Vector[A] => State[S, Vector[A]])
  (initialPopulation: State[S, Vector[A]])
  (generationCount: Int)
  : State[S, Vector[A]] = {
      evolveOverGenerationsStateful(generateNextPopulation)(generationCount)(initialPopulation)
  }

  def getBestStrategy[S,A]
  (getFitness: A => State[S, Int])
  (population: Vector[A])
  : State[S, (A, Int)] = {
    val pAndF: Vector[State[S, (A, Int)]] =
      population.map(p => getFitness(p).map(i => (p,i)))
    val a = State.sequenceV(pAndF)
    val best = a.map(x => x.sortBy(e => e._2).last)
    best
  }
}
