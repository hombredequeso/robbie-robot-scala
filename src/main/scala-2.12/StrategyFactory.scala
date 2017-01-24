package com.hombredequeso.robbierobot

import Strategy._
import com.hombredequeso.robbierobot.Action.Action
import scala.util.Random.nextInt

object StrategyFactory {

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

  def initStrategy(action: Action): StrategyMap = {
    val r = allScenarios.toList.map(s => (s -> action)).toMap
    r
  }
}
