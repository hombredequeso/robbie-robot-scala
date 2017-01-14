package com.hombredequeso.robbierobot

import Strategy._
import com.hombredequeso.robbierobot.Action.Action
import scala.util._

object Evolve {


  case class Member(val strategy: StrategyMap, val fitness: Int){}

  def getWeightedValue[T](
         population: Vector[(Int,T)],
         accumulatedWeight: Int,
         targetWeight: Int,
         baseWeight: Int): T = {
    val newAccumulatedWeight = accumulatedWeight + (population.head._1 - baseWeight + 1)
    if (newAccumulatedWeight >= targetWeight || population.length == 1)
      population.head._2
    else
      getWeightedValue(
        population.tail,
        newAccumulatedWeight,
        targetWeight,
        baseWeight)
  }


  def getWeightedRandom[T](population: Vector[(Int, T)]): T = {
    // todo: what if no elements in vector?
    val baseWeight = population.map(_._1).min
    val totalWeight = population.map(x => (x._1 - baseWeight + 1)).sum
    val targetWeight = new Random().nextInt(totalWeight)+1
    getWeightedValue(population, 0, targetWeight, baseWeight)
  }

  def getBreedingMembers(population: Vector[Member]): (StrategyMap, StrategyMap) = {
    // remap weightings:
    val pop2: Vector[(Int, Member)] = population.map(x => (x.fitness, x))
    (getWeightedRandom(pop2).strategy, getWeightedRandom(pop2).strategy)
  }

  def breed(breedingMembers: (StrategyMap, StrategyMap)): StrategyMap = {
    val x = breedingMembers._1.toVector.sortBy(x => x._1)
    val size = x.length
    val randomPoint = new Random().nextInt(size)
    val part1 = x.slice(0, randomPoint)
    val y = breedingMembers._2.toVector.sortBy(x => x._1)
    val part2 = y.slice(randomPoint, y.length + 1)
    val result = part1 ++: part2
    val r2 = result.toMap
    r2
  }

  type VectorizedStrategy = Vector[(Scenario, Action)]

  def mutateR(r: Random)(strategy: VectorizedStrategy, mutateCount: Int): VectorizedStrategy = {
    mutateCount match {
      case x if x <= 0 => strategy
      case _ => {
        val itemPosTomutate = r.nextInt(strategy.size)
        val itemToMutate = strategy(itemPosTomutate)
        val randomAction = Action(r.nextInt(Action.maxId))
        val newOne = strategy.patch(itemPosTomutate, Vector((itemToMutate._1, randomAction)), 1)
        mutateR(r)(newOne, mutateCount - 1)
      }
    }
  }

  def mutate(strategy: StrategyMap): StrategyMap = {
    val r = new Random()
    val percentageToRandomlyMutate = 5
    val countToMutate = strategy.size * percentageToRandomlyMutate
    val result = mutateR(r)(strategy.toVector, countToMutate)
    result.toMap
  }

  def evolveNewMember(population: Vector[Member]) : StrategyMap = {
    val breedingMembers = getBreedingMembers(population)
    val newMember1 = breed(breedingMembers)
    val newMember2 = mutate(newMember1)
    newMember2
  }

  def evolve(population: Vector[Member]): Vector[StrategyMap] = {
    val totalWeight = population.map(x => (x.fitness)).sum
    val minWeight = population.map(x => (x.fitness)).min
    val maxWeight = population.map(x => (x.fitness)).max
    Console.println(s"max = ${maxWeight}; min = ${minWeight}; totalWeight = ${totalWeight}")

    (1 to population.length).par.map(_ => evolveNewMember(population)).toVector
  }
  def generateNextPopulation
  (getFitness: StrategyMap => Int)
  (population: Vector[StrategyMap])
  : Vector[StrategyMap] = {
    val members: Vector[Member] =
      population.map(
        s => Member(s, getFitness(s)))
    val newPopulation = Evolve.evolve(members)
    newPopulation
  }
}

