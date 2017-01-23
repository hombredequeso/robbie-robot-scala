package com.hombredequeso.robbierobot.Ui

import com.hombredequeso.robbierobot.{Board, Play}
import com.hombredequeso.robbierobot.Play.{Coord, PlayStrategy, State}
import com.hombredequeso.robbierobot.Strategy.StrategyMap
import com.hombredequeso.util.RND.ScalaRandomizer
import com.hombredequeso.util.RandomProvider

object GamePlay {

  def InitializeGameState(randomizer: RandomProvider)(xSquareCount: Int, ySquareCount: Int, robotPosition: Coord) = {
    gameState = State(
      Board.createRandomBoard(randomizer)(xSquareCount, ySquareCount, 0.5f).map(x => x.toList).toList,
      Play.initialRobotPosition
    )
  }
  var gameState: Play.State = State(
    Board.createRandomBoard(new ScalaRandomizer())(0, 0, 0.5f).map(x => x.toList).toList,
    Coord(0,0)
  )
  val r = new ScalaRandomizer()

  def executeTurn(strategy: StrategyMap) = {
    val startState = gameState
    val playStrategy = PlayStrategy(strategy, r)
    val (newState, _) = Play.executeTurn(playStrategy)(startState)
    gameState = State(
      newState.board.map(x => x.toList).toList,
      newState.robotPos
    )
  }
}
