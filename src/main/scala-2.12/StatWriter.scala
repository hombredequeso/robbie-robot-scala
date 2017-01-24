package com.hombredequeso.robbierobot

import com.hombredequeso.robbierobot.Evolve.Member
import com.hombredequeso.robbierobot.Strategy.StrategyMap

object StatWriter {

  var iteration = 0

  def write(population: Vector[Member[StrategyMap]]): Unit = {
    val totalWeight = population.map(x => (x.fitness)).sum
    val minWeight = population.map(x => (x.fitness)).min
    val maxWeight = population.map(x => (x.fitness)).max
    Console.println(s"it = ${iteration}: max = ${maxWeight}; min = ${minWeight}; totalWeight = ${totalWeight}")
    iteration = iteration + 1
  }
}
