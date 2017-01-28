package com.hombredequeso.robbierobot

import Strategy._
import com.hombredequeso.robbierobot.Action.Action
import com.hombredequeso.util.RND.ScalaRandomizer
import com.hombredequeso.util.{RND, RandomProvider, State}

import scala.math._

object Evolve {

  case class Member[A](val strategy: A, val fitness: Int){}

  def getBreedingMemberB[A]
  (membersToRandomlyPick: Int)
  (population: Vector[Member[A]])
  (r: RandomProvider)
  : (Member[A], RandomProvider) = {
    val populationSize = population.length
    val (randomIndices, nextRandomProvider) =
      RND.nextN(membersToRandomlyPick)(r => r.nextInt(populationSize))(r)
    (randomIndices.map(i => population(i)).maxBy(x => x.fitness), nextRandomProvider)
  }

  def getBreedingMemberM[A]
  (membersToRandomlyPick: Int)
  (population: Vector[Member[A]])
  : State[RandomProvider, Member[A]] = {
    State(getBreedingMemberB(membersToRandomlyPick)(population))
  }

  def getBreedingMembers[A]
  (population: Vector[Member[A]])
  : State[RandomProvider, (A, A)] = {
    val ratioOfMembersToRandomlyPick = 0.15
    val membersToRandomlyPick = Math.ceil(population.length * ratioOfMembersToRandomlyPick).toInt
    for {
      result1 <- getBreedingMemberM(membersToRandomlyPick)(population)
      result2 <- getBreedingMemberM(membersToRandomlyPick)(population)
    }
      yield(result1.strategy, result2.strategy)
  }

  def breed[A,B]
  (breedingMembers: (Map[A,B], Map[A,B]))
  (randomizer: RandomProvider)
  (implicit ordering:Ordering[A])
  : (Map[A,B], RandomProvider) = {
    val x = breedingMembers._1.toVector.sortBy(x => x._1)
    val size = x.length
    val randomPoint = randomizer.nextInt(size)._1
    val part1 = x.slice(0, randomPoint)
    val y = breedingMembers._2.toVector.sortBy(x => x._1)
    val part2 = y.slice(randomPoint, y.length + 1)
    val result = part1 ++: part2
    val r2 = result.toMap
    (r2, randomizer)
  }

  type VectorizedStrategy = Vector[(Scenario, Action)]

  def mutateR
  (r: RandomProvider)
  (strategy: VectorizedStrategy, mutateCount: Int)
  : VectorizedStrategy = {
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

  def mutate
  (strategy: StrategyMap)
  (r: RandomProvider)
  : (StrategyMap, RandomProvider) = {
    val ratioToMutate = 0.15
    val countToMutate = Math.ceil(strategy.size.toDouble * ratioToMutate).toInt
    val actualCountToMutate = r.nextInt(countToMutate + 1)._1
    val result = mutateR(r)(strategy.toVector, actualCountToMutate)
    (result.toMap, r)
  }

  def evolveNewMember
  (population: Vector[Member[StrategyMap]])
  : State[RandomProvider, StrategyMap] = {
    for {
      gbm <- getBreedingMembers(population)
      b1 <- State(breed(gbm))
      m <- State(mutate(b1))
    } yield m
  }

  def evolve
  (randomizer: RandomProvider)
  (population: Vector[Member[StrategyMap]])
  : Vector[StrategyMap] = {
    val result = (1 to population.length)
      .par
      .map(_ => evolveNewMember(population).run(new ScalaRandomizer(randomizer.nextInt()._1)))
      .toVector

    // For the moment throw away state:
    result.map(r => r._1)
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

