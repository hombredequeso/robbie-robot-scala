package com.hombredequeso.robbierobot

import com.hombredequeso.robbierobot.Evolve.Member
import com.hombredequeso.robbierobot.Strategy.StrategyMap

object StatWriter {

  var iteration = 0

  def getStats(population: Vector[Member[StrategyMap]]): String = {
    val totalWeight = population.map(x => (x.fitness)).sum
    val minWeight = population.map(x => (x.fitness)).min
    val maxWeight = population.map(x => (x.fitness)).max
      s"it = ${iteration}: max = ${maxWeight}; min = ${minWeight}; totalWeight = ${totalWeight}"
  }

  def write(population: Vector[Member[StrategyMap]]): Unit = {
    val statLine = getStats(population)
    Console.println(statLine)
    iteration = iteration + 1
  }
}
