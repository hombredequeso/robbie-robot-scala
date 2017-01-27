package com.hombredequeso.robbierobot

import Strategy._
import com.hombredequeso.robbierobot.Action.Action
import com.hombredequeso.util.RND.ScalaRandomizer
import com.hombredequeso.util.{RandomProvider, State}

import scala.math._

object Evolve {

  case class Member[A](val strategy: A, val fitness: Int){}

  def getBreedingMember[A](r: RandomProvider)(membersToRandomlyPick: Int)(population: Vector[Member[A]]): Member[A] = {
    val populationSize = population.length
    Seq.fill(membersToRandomlyPick)(population(r.nextInt(populationSize)._1))
      .maxBy(x => x.fitness)
  }

  def getBreedingMembers[A](population: Vector[Member[A]])(r: RandomProvider): ((A, A), RandomProvider) = {
    val ratioOfMembersToRandomlyPick = 0.2
    val membersToRandomlyPick = Math.ceil(population.length * ratioOfMembersToRandomlyPick).toInt;
    val result1 = getBreedingMember(r)(membersToRandomlyPick)(population)
    val result2 = getBreedingMember(r)(membersToRandomlyPick)(population)
    ((result1.strategy, result2.strategy), r)
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

  def mutate
  (strategy: StrategyMap)
  (r: RandomProvider)
  : (StrategyMap, RandomProvider) = {
    val ratioToMutate = 0.2
    val countToMutate = (strategy.size.toDouble * ratioToMutate).toInt
    val actualCountToMutate = r.nextInt(countToMutate + 1)._1
    val result = mutateR(r)(strategy.toVector, actualCountToMutate)
    (result.toMap, r)
  }

  def evolveNewMember(population: Vector[Member[StrategyMap]])(randomizer: => RandomProvider) : StrategyMap = {
    val (breedingMembers, randomizer2) = getBreedingMembers(population)(randomizer)
    val (newMember1, randomizer3) = breed(breedingMembers)(randomizer2)
    val (newMember2, randomizer4) = mutate(newMember1)(randomizer3)
    newMember2
  }

  def evolveNewMemberMFor2
  (population: Vector[Member[StrategyMap]])
  : State[RandomProvider, StrategyMap] = {
    for {
      gbm <- State(getBreedingMembers(population))
      b1 <- State(breed(gbm))
      m <- State(mutate(b1))
    } yield m
  }

  def evolveNewMemberMFor1
  (population: Vector[Member[StrategyMap]])
  (randomizer: => RandomProvider)
  : (StrategyMap, RandomProvider) = {
    def result = for{
      gbm <- State(getBreedingMembers(population))
      b1 <- State(breed(gbm))
      m <- State(mutate(b1))
    } yield m
    result.run(randomizer)
  }

  def evolveNewMemberMFlatMap
  (population: Vector[Member[StrategyMap]])
  (randomizer: => RandomProvider)
  : (StrategyMap, RandomProvider) = {
    def resultM2 = State(getBreedingMembers(population))
      .flatMap(gbm => State(breed(gbm)))
      .flatMap(b1 => State(mutate(b1)))

    resultM2.run(randomizer)
  }

  def evolveOld
  (randomizer: RandomProvider)
  (population: Vector[Member[StrategyMap]])
  : Vector[StrategyMap] = {
    (1 to population.length)
      .par
      .map(_ => evolveNewMember(population)(new ScalaRandomizer(randomizer.nextInt()._1)))
      .toVector
  }

  def evolve
  (randomizer: RandomProvider)
  (population: Vector[Member[StrategyMap]])
  : Vector[StrategyMap] = {
    val result = (1 to population.length)
      .par
      .map(_ => evolveNewMemberMFor2(population).run(new ScalaRandomizer(randomizer.nextInt()._1)))
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

