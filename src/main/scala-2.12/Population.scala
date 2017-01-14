package com.hombredequeso.robbierobot

import Strategy._
import com.hombredequeso.robbierobot.Action.Action

object Evolve {

  import util.Random.nextInt

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

  def getTotalWeight[T](population: Vector[(Int, T)]): Int = {
    val orderedPopulation = population.sortBy(x => x._1)
    val minWeight = orderedPopulation(0)._1
    val totalWeight = population.map(x => (x._1 - minWeight + 1)).sum
    totalWeight
  }

  def getWeightedRandom[T](population: Vector[(Int, T)]): T = {
    // todo: what if no elements in vector?
    val baseWeight = population.sortBy(x => x._1).head._1
    val totalWeight = getTotalWeight(population)
    val targetWeight = nextInt(totalWeight) + 1
    val result = getWeightedValue(population, 0, targetWeight, baseWeight)
    result
  }

  def getBreedingMembers(population: Vector[Member]): (StrategyMap, StrategyMap) = {
    val pop2 = population.map(x => (x.fitness, x))
    (getWeightedRandom(pop2).strategy, getWeightedRandom(pop2).strategy)
  }

  def breed(breedingMembers: (StrategyMap, StrategyMap)): StrategyMap = {
    val x = breedingMembers._1.toVector.sortBy(x => x._1)
    val size = x.length
    val randomPoint = nextInt(size)
    val part1 = x.slice(0, randomPoint)
    val y = breedingMembers._2.toVector.sortBy(x => x._1)
    val part2 = y.slice(randomPoint, y.length + 1)
    val result = part1 ++: part2
    val r2 = result.toMap
    r2
  }

  type VectorizedStrategy = Vector[(Scenario, Action)]

  def mutateR(strategy: VectorizedStrategy, mutateCount: Int): VectorizedStrategy = {
    mutateCount match {
      case x if x <= 0 => strategy
      case _ => {
        val itemPosTomutate = nextInt(strategy.size)
        val itemToMutate = strategy(itemPosTomutate)
        val randomAction = Action(nextInt(Action.maxId))
        val newOne = strategy.patch(itemPosTomutate, Vector((itemToMutate._1, randomAction)), 1)
        mutateR(newOne, mutateCount - 1)
      }
    }
  }

  def mutate(strategy: StrategyMap): StrategyMap = {
    val percentageToRandomlyMutate = 2
    val countToMutate = strategy.size * percentageToRandomlyMutate
    val result = mutateR(strategy.toVector, countToMutate)
    result.toMap
  }

  def evolveNewMember(population: Vector[Member]) : StrategyMap = {
    val breedingMembers = getBreedingMembers(population)
    val newMember1 = breed(breedingMembers)
    val newMember2 = mutate(newMember1)
    newMember2
  }

  def evolve(population: Vector[Member]): Vector[StrategyMap] = {
    (1 to population.length).map(_ => evolveNewMember(population)).toVector
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

