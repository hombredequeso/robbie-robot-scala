package com.hombredequeso.robbierobot

import Strategy._

object StrategyFactory {

  import util.Random.nextInt
  def createRandomActions() = {
    Stream.continually(Action(nextInt(Action.maxId)))
  }

  val allScenarios =
    for {
      n <- Content.values
      s <- Content.values
      e <- Content.values
      w <- Content.values
      c <- Content.values
    } yield Scenario(n, s, e, w, c)

  def make(scenarios: Set[Scenario], actions: Stream[Action.Value]) : StrategyMap = {
    scenarios zip actions toMap
  }

  def createInitialPopulation(populationSize: Int): Vector[StrategyMap] = {
    (1 to populationSize).map(x =>
      StrategyFactory.make(
        StrategyFactory.allScenarios,
        StrategyFactory.createRandomActions())
    ).toVector
  }
}
