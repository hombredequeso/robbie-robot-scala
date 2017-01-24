package com.hombredequeso.robbierobot

import Strategy._
import com.hombredequeso.robbierobot.Action.Action
import com.hombredequeso.util.RND.ScalaRandomizer
import com.hombredequeso.util.RandomProvider

import scala.math._

object Evolve {

  case class Member[A](val strategy: A, val fitness: Int){}

  def getBreedingMember[A](r: RandomProvider)(membersToRandomlyPick: Int)(population: Vector[Member[A]]): Member[A] = {
    val populationSize = population.length
    Seq.fill(membersToRandomlyPick)(population(r.nextInt(populationSize)._1))
      .maxBy(x => x.fitness)
  }

  def getBreedingMembers[A](r: RandomProvider)(population: Vector[Member[A]]): (A, A) = {
    val ratioOfMembersToRandomlyPick = 0.1
    val membersToRandomlyPick = Math.ceil(population.length * ratioOfMembersToRandomlyPick).toInt;
    val result1 = getBreedingMember(r)(membersToRandomlyPick)(population)
    val result2 = getBreedingMember(r)(membersToRandomlyPick)(population)
    (result1.strategy, result2.strategy)
  }

  def breed[A,B](randomizer: RandomProvider)(breedingMembers: (Map[A,B], Map[A,B]))(implicit ordering:Ordering[A]): Map[A,B] = {
    val x = breedingMembers._1.toVector.sortBy(x => x._1)
    val size = x.length
    val randomPoint = randomizer.nextInt(size)._1
    val part1 = x.slice(0, randomPoint)
    val y = breedingMembers._2.toVector.sortBy(x => x._1)
    val part2 = y.slice(randomPoint, y.length + 1)
    val result = part1 ++: part2
    val r2 = result.toMap
    r2
  }

  type VectorizedStrategy = Vector[(Scenario, Action)]

  def mutateR(r: RandomProvider)(strategy: VectorizedStrategy, mutateCount: Int): VectorizedStrategy = {
    mutateCount match {
      case x if x <= 0 => strategy
      case _ => {
        val itemPosToMutate = r.nextInt(strategy.size)._1
        val itemToMutate = strategy(itemPosToMutate)
        val randomAction = Action(r.nextInt(Action.maxId)._1)
        val newOne = strategy.patch(itemPosToMutate, Vector((itemToMutate._1, randomAction)), 1)
        mutateR(r)(newOne, mutateCount - 1)
      }
    }
  }

  def mutate(r: RandomProvider)(strategy: StrategyMap): StrategyMap = {
    val ratioToMutate = 0.1
    val countToMutate = (strategy.size.toDouble * ratioToMutate).toInt
    val actualCountToMutate = r.nextInt(countToMutate + 1)._1
    val result = mutateR(r)(strategy.toVector, actualCountToMutate)
    result.toMap
  }

  def evolveNewMember(randomizer: => RandomProvider)(population: Vector[Member[StrategyMap]]) : StrategyMap = {
    val breedingMembers = getBreedingMembers(randomizer)(population)
    val newMember1 = breed(randomizer)(breedingMembers)
    val newMember2 = mutate(randomizer)(newMember1)
    newMember2
  }

  def evolve
  (randomizer: RandomProvider)
  (population: Vector[Member[StrategyMap]])
  : Vector[StrategyMap] = {
    (1 to population.length)
      .par
      .map(_ => evolveNewMember(new ScalaRandomizer(randomizer.nextInt()._1))(population))
      .toVector
  }

  def generateNextPopulation
  (randomizer: RandomProvider)
  (statWriter: Vector[Member[StrategyMap]] => Unit)
  (getFitness: StrategyMap => Int)
  (population: Vector[StrategyMap])
  : Vector[StrategyMap] = {
    val members: Vector[Member[StrategyMap]] =
      population.map(
        s => Member(s, getFitness(s)))
    statWriter(members)
    val newPopulation = Evolve.evolve(randomizer)(members)
    newPopulation
  }
}

