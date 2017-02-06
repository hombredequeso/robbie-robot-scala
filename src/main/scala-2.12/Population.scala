package com.hombredequeso.robbierobot

import Strategy._
import com.hombredequeso.robbierobot.Action.Action
import com.hombredequeso.util.RND.ScalaRandomizer
import com.hombredequeso.util.{RND, RandomProvider, State}

import scala.math._

object Evolve {

  object RndState {
    def nextInt(size: Int)= State[RandomProvider, Int](_.nextInt(size))
    def nextInt = State[RandomProvider, Int](_.nextInt())

    def nextN[A](count: Int)(getNext: RandomProvider => (A, RandomProvider)) =
      State[RandomProvider, List[A]](r =>  r.nextN(count)(getNext))
  }

  case class GenerationStatistics(val lines: List[String], val generation: Int) {
    def newGeneration(): GenerationStatistics = {
      val newGenerationNumber = generation + 1
      val newStatLine = s"Generation ${newGenerationNumber}"
      Console.println(newStatLine)
      this.copy(generation = newGenerationNumber, lines = newStatLine::lines)
    }

    def write(x: Vector[Member[StrategyMap]]): GenerationStatistics = {
      val newStatLine = StatWriter.getStats(x)
      Console.println(newStatLine)
      val newStatLines = newStatLine :: lines
      this.copy(lines = newStatLines)
    }
  }

  object StatisticsState {
    def write(x: Vector[Member[StrategyMap]])=
      State[GenerationStatistics, Unit](s => (Unit, s.write(x)))
    def newGeneration() =
      State[GenerationStatistics, Unit](s => (Unit, s.newGeneration()))
  }

  case class GenerationContext
  (
    val randomizer: RandomProvider,
    val statWriter: GenerationStatistics
  )

  case class Member[A](val strategy: A, val fitness: Int) {}

  def getBreedingMember[A]
  (membersToRandomlyPick: Int)
  (population: Vector[Member[A]])
  :State[RandomProvider, Member[A]] = {
    val populationSize = population.length
    for {
      randomIndices <- RndState.nextN(membersToRandomlyPick)(r => r.nextInt(populationSize))
    } yield
      randomIndices.map(i => population(i)).maxBy(x => x.fitness)
  }

  def getBreedingMembers[A]
  (population: Vector[Member[A]])
  : State[RandomProvider, (A, A)] = {
    val ratioOfMembersToRandomlyPick = 0.15
    val membersToRandomlyPick = Math.ceil(population.length * ratioOfMembersToRandomlyPick).toInt
    for {
      result1 <- getBreedingMember(membersToRandomlyPick)(population)
      result2 <- getBreedingMember(membersToRandomlyPick)(population)
    }
      yield (result1.strategy, result2.strategy)
  }

  def breed[A, B]
  (breedingMembers: (Map[A, B], Map[A, B]))
  (implicit ordering: Ordering[A]):
    State[RandomProvider, Map[A, B]] = {
    val x = breedingMembers._1.toVector.sortBy(x => x._1)
    val size = x.length
    for {
      randomPoint <- RndState.nextInt(size)
      part1 = x.slice(0, randomPoint)
      y = breedingMembers._2.toVector.sortBy(x => x._1)
      part2 = y.slice(randomPoint, y.length + 1)
      result = part1 ++: part2
      r2 = result.toMap
    } yield r2
  }

  type VectorizedStrategy = Vector[(Scenario, Action)]

  def mutateIt(itemPosToMutate: Int) =
    State[(RandomProvider, VectorizedStrategy), Unit](s => {
      val (randomizer, strategy) = s
      val (randomActionIndex, nextRandomizer) = randomizer.nextInt(Action.maxId)
      val itemToMutate = strategy(itemPosToMutate)
      val randomAction = Action(randomActionIndex)
      val result = strategy.patch(itemPosToMutate, Vector((itemToMutate._1, randomAction)), 1)
      (Unit, (nextRandomizer, result))
    })

  def mutate
  (strategy: StrategyMap)
  = State[RandomProvider, StrategyMap](r => {
    val ratioToMutate = 0.15
    val strategySize = strategy.size
    val countToMutate = Math.ceil(strategy.size.toDouble * ratioToMutate).toInt
    val (actualCountToMutate, randomProvider2) = r.nextInt(countToMutate + 1)
    val (indexesToMutate, randomProvider3) =
      RND.nextN(actualCountToMutate)(r => r.nextInt(strategySize))(randomProvider2)
    val funcsToMutate = indexesToMutate.map(i => mutateIt(i))

    val result: State[(RandomProvider, VectorizedStrategy), None.type] = for {
      _ <- State.sequence(funcsToMutate)
      s <- State.get
    } yield None
    val startState = (randomProvider3, strategy.toVector)
    val (_, (rn, vs)) = result.run(startState)
    (vs.toMap, rn)
  })

  def evolveNewMember
  (population: Vector[Member[StrategyMap]])
  : State[RandomProvider, StrategyMap] =
    for {
      parents <- getBreedingMembers(population)
      child <- breed(parents)
      mutatedChild <- mutate(child)
    } yield mutatedChild

  def evolve
  (population: Vector[Member[StrategyMap]])
  : State[RandomProvider, Vector[StrategyMap]] = for {
    parallelRandomizers <- State[RandomProvider, List[Int]] (
      RND.nextN(population.length)(p => p.nextInt()))
    result = parallelRandomizers
      .par
      .map(x => evolveNewMember(population).run(new ScalaRandomizer(x))._1)
      .toVector
  } yield result


  def generateNextPopulation
  (getFitness: StrategyMap => Int)
  (population: Vector[StrategyMap])
  = State[GenerationContext, Vector[StrategyMap]]((context: GenerationContext) => {
    val members: Vector[Member[StrategyMap]] =
      population.map(s => Member(s, getFitness(s)))

    val statResult = for {
      r1 <- StatisticsState.newGeneration()
      r2 <- StatisticsState.write(members)
    } yield r2

    val statFinally = statResult.run(context.statWriter)._2

    val (newPopulation, nextRandomizer) = Evolve.evolve(members).run(context.randomizer)
    (newPopulation, GenerationContext(nextRandomizer, statFinally))
  })
}

