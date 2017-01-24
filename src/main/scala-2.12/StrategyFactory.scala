package com.hombredequeso.robbierobot

import Strategy._
import Action.Action
import com.hombredequeso.util.RandomProvider

object StrategyFactory {

  def createRandomActions(randomizer: RandomProvider) = {
    Stream.continually(Action(randomizer.nextInt(Action.maxId)))
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

  def createInitialPopulation(randomizer: RandomProvider)(populationSize: Int): Vector[StrategyMap] = {
    (1 to populationSize).map(x =>
      StrategyFactory.make(
        StrategyFactory.allScenarios,
        StrategyFactory.createRandomActions(randomizer))
    ).toVector
  }

  def initStrategy(action: Action): StrategyMap = {
    val r = allScenarios.toList.map(s => (s -> action)).toMap
    r
  }
}
