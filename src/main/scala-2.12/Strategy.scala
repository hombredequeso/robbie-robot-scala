package com.hombredequeso.robbierobot

import com.hombredequeso.robbierobot.Action._
import com.hombredequeso.robbierobot.Content._
import com.hombredequeso.util.RND.ScalaRandomizer
import com.hombredequeso.util.{RND, RandomProvider}

import scala.math.pow

object Strategy {

  case class Scenario(
                       val north: Content,
                       val south: Content,
                       val east: Content,
                       val west: Content,
                       val current: Content
                     ) extends Ordered[Scenario]
  {
    def toOrderValue(): Double = {
      val max = Content.maxId
      north.id +
        (south.id * max) +
        (east.id * pow(max, 2)) +
        (west.id * pow(max, 3)) +
        (current.id * pow(max, 4))
    }
    override def compare(that: Scenario): Int = {
      val x1 = this.toOrderValue()
      val y1 = that.toOrderValue()
      x1.compare(y1)
    }
  }

  type StrategyMap = Map[Scenario, Action]

  // gets the fitness of a strategy, taking a RandomProvider (stateful),
  //  and returns the new stateful RandomProvider
  def getStrategyFitness
  (randomizer: RandomProvider)
  (boardCount: Int, numberOfTurnsPerBoard: Int)
  (strategy: StrategyMap)
  : (Int, RandomProvider) = {
    val (randomSeeds, newRandomizer) = RND.nextInts(boardCount)(randomizer)
    val boards = randomSeeds
      .par
      .map(x => Board.createRandomBoard(new ScalaRandomizer(x))(10, 10, 0.5f))
    val r = new ScalaRandomizer()
    val fitness = boards.map(b =>
      Play.execute(randomizer)(
        Play.State(b, Play.initialRobotPosition),
        strategy,
        numberOfTurnsPerBoard))
    (fitness.sum, newRandomizer)
  }
}

