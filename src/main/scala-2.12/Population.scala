package com.hombredequeso.robbierobot

import Strategy._
import com.hombredequeso.robbierobot.Action.Action
import scala.util._
import scala.math._

object Evolve {

  case class Member[A](val strategy: A, val fitness: Int){}

  def getBreedingMember[A](r: Random)(membersToRandomlyPick: Int)(population: Vector[Member[A]]): Member[A] = {
    val populationSize = population.length
    Seq.fill(membersToRandomlyPick)(population(r.nextInt(populationSize)))
      .maxBy(x => x.fitness)
  }

  def getBreedingMembers[A](r: Random)(population: Vector[Member[A]]): (A, A) = {
    val membersToRandomlyPick = 15;
    val result1 = getBreedingMember(r)(membersToRandomlyPick)(population)
    val result2 = getBreedingMember(r)(membersToRandomlyPick)(population)
    (result1.strategy, result2.strategy)
  }

  def breed[A,B](breedingMembers: (Map[A,B], Map[A,B]))(implicit ordering:Ordering[A]): Map[A,B] = {
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
        val itemPosToMutate = r.nextInt(strategy.size)
        val itemToMutate = strategy(itemPosToMutate)
        val randomAction = Action(r.nextInt(Action.maxId))
        val newOne = strategy.patch(itemPosToMutate, Vector((itemToMutate._1, randomAction)), 1)
        mutateR(r)(newOne, mutateCount - 1)
      }
    }
  }

  def mutate(strategy: StrategyMap): StrategyMap = {
    val r = new Random()
    val ratioToMutate = 0.2
    val countToMutate = (strategy.size.toDouble * ratioToMutate).toInt
    val actualCountToMutate = r.nextInt(countToMutate + 1)
    val result = mutateR(r)(strategy.toVector, actualCountToMutate)
    result.toMap
  }

  def evolveNewMember(population: Vector[Member[StrategyMap]]) : StrategyMap = {
    val breedingMembers = getBreedingMembers(new Random())(population)
    val newMember1 = breed(breedingMembers)
    val newMember2 = mutate(newMember1)
    newMember2
  }

  def evolve(population: Vector[Member[StrategyMap]]): Vector[StrategyMap] = {
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
    val members: Vector[Member[StrategyMap]] =
      population.map(
        s => Member(s, getFitness(s)))
    val newPopulation = Evolve.evolve(members)
    newPopulation
  }
}

