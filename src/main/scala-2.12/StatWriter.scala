package com.hombredequeso.robbierobot

import com.hombredequeso.robbierobot.Evolve.Member
import com.hombredequeso.robbierobot.Strategy.StrategyMap

object StatWriter {

  def getStats(population: Vector[Member[StrategyMap]]): String = {
    val totalWeight = population.map(x => (x.fitness)).sum
    val minWeight = population.map(x => (x.fitness)).min
    val maxWeight = population.map(x => (x.fitness)).max
      s"Population Health: max = ${maxWeight}; min = ${minWeight}; totalWeight = ${totalWeight}"
  }
}
