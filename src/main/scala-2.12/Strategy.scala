package com.hombredequeso.robbierobot

import Content._
import Action._
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

  def getStrategyFitness
  (boardCount: Int, numberOfTurnsPerBoard: Int)
  (strategy: StrategyMap)
  : Int = {
    val boards = (1 to boardCount).par.map(x => Board.createRandomBoard(100, 100, 0.5f))
    val fitness = boards.map(b =>
      Play.execute(
        Play.State(b, Play.initialRobotPosition),
        strategy,
        numberOfTurnsPerBoard))
    fitness.sum
  }
}

