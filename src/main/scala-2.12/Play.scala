package com.hombredequeso.robbierobot

import com.hombredequeso.robbierobot.Board.Board
import com.hombredequeso.robbierobot.Strategy._
import com.hombredequeso.util.RandomProvider

object Play {

  object RelativePosition extends Enumeration {
    type RelativePosition = Value
    val North, South, East, West, Current = Value
  }

  import RelativePosition._
  import Action._

  case class Coord(val x: Int, val y: Int){
    def + (c: Coord): Coord = {
      Coord(this.x + c.x, this.y + c.y)
    }
  }
  case class State(val board: Board, val robotPos: Coord){}

  val initialRobotPosition = Coord(4,4)

  case class PlayStrategy(val map: StrategyMap, val random: RandomProvider){}

  def execute(r: RandomProvider)(state: State, strategy: StrategyMap, turns: Int): Int = {
    val doTurn = executeTurn(PlayStrategy(strategy, r))_
    val endResult = (1 to turns).foldLeft((state, 0))((current,_) => {
      val next = doTurn(current._1)
      (next._1, next._2 + current._2)
    })
    endResult._2
  }

  // returns (newState, turnScore
  def executeTurn(strategy: PlayStrategy)(state: State) : (State, Int) = {
    val scenario: Scenario = getScenario(state)
    val move: Action.Value = strategy.map(scenario)
    val moveResult: (State, Int) = executeAction(strategy.random)(state, move)
    moveResult
  }

  def executeMove(state: State, move: Coord): (State, Int) = {
    val newPos = state.robotPos + move
    val didHitWall: Boolean = outsideBoardLimit(state.board, newPos)
    if (didHitWall) {
      (state, Scores.HitWall)
    } else {
      (State(state.board, newPos), Scores.None)
    }
  }

  val scenarioOffsets: Map[RelativePosition, Coord] =
    Map(
      North -> Coord(0,-1),
      South -> Coord(0,1),
      East -> Coord(1, 0),
      West -> Coord(-1, 0),
      Current -> Coord(0,0) )

  def getContent(board: Board, c: Coord): Content.Value = {
    board
      .lift(c.x).getOrElse(Vector())
      .lift(c.y).getOrElse(Content.Wall)
  }

  def getScenario(state: State) : Scenario = {
    Scenario(
      getContent(state.board, state.robotPos + scenarioOffsets(North)),
      getContent(state.board, state.robotPos + scenarioOffsets(South)),
      getContent(state.board, state.robotPos + scenarioOffsets(East)),
      getContent(state.board, state.robotPos + scenarioOffsets(West)),
      getContent(state.board, state.robotPos))
  }

  object Scores {
    val None = 0
    val PickedUpCan = 10
    val DidNotPickupCan = -1
    val HitWall = -5
  }


  def outsideBoardLimit(board: Board, newPos: Coord): Boolean = {
    val content = board
                    .lift(newPos.x).getOrElse(Vector())
                    .lift(newPos.y).getOrElse(Content.Wall)
    content == Content.Wall
  }

  def getRandomMove(r: RandomProvider) = {
    val x = r.nextInt(4)._1
    x match {
      case 0 => North
      case 1 => South
      case 2 => East
      case 3 => West
    }
  }

  def executeAction(random: RandomProvider)(state: State, action: Action.Value): (State, Int) = {
    action match {
      case MoveNorth =>
        executeMove(state, scenarioOffsets(North))
      case MoveSouth =>
        executeMove(state, scenarioOffsets(South))
      case MoveEast =>
        executeMove(state, scenarioOffsets(East))
      case MoveWest =>
        executeMove(state, scenarioOffsets(West))
      case MoveRandom => {
        executeMove(state, scenarioOffsets(getRandomMove(random)))
      }
      case Nothing =>
        (state, Scores.None)
      case PickUpCan => {
        val canPickupCan = state.board(state.robotPos.x)(state.robotPos.y) == Content.Can
        if (canPickupCan) {
          val replacementYBit = state.board(state.robotPos.x).updated(state.robotPos.y, Content.Empty)
          val newBoard = state.board.updated(state.robotPos.x, replacementYBit)
          (State(newBoard, state.robotPos), Scores.PickedUpCan)
        } else {
          (state, Scores.DidNotPickupCan)
        }
      }
    }
  }

}
