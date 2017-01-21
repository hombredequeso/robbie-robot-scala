package Ui

import scala.util.Random
import com.hombredequeso.robbierobot.{Board, Play}
import com.hombredequeso.robbierobot.Play.{Coord, PlayStrategy, State}
import com.hombredequeso.robbierobot.Strategy.StrategyMap

object GamePlay {

  def InitializeGameState(xSquareCount: Int, ySquareCount: Int, robotPosition: Coord) = {
    gameState = State(
      Board.createRandomBoard(xSquareCount, ySquareCount, 0.5f).map(x => x.toList).toList,
      Play.initialRobotPosition
    )
  }
  var gameState: Play.State = State(
    Board.createRandomBoard(0, 0, 0.5f).map(x => x.toList).toList,
    Coord(0,0)
  )
  val r = new Random()

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
