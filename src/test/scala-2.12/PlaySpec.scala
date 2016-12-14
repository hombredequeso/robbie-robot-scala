package com.hombredequeso.robbierobot


import com.hombredequeso.robbierobot.Play.State
import org.scalatest.FunSpec



class PlaySpec extends FunSpec {

  describe("executeTurn") {
    describe("with random input data") {
      it("should return new state and turn score which is higher than lowest possible score") {
        val strategy = StrategyFactory.make(
          StrategyFactory.allScenarios,
          StrategyFactory.createRandomActions())
        val board = Board.createRandomBoard(100, 100, 0.5f)
        val state = State(board, Play.initialRobotPosition)

        val result = Play.executeTurn(strategy)(state)

        assert(result._2 >= Play.Scores.HitWall)
      }
    }
  }

  describe("execute") {
    describe("with random input data") {
      it("should return higher than theoretical minimum score") {
        val strategy = StrategyFactory.make(
          StrategyFactory.allScenarios,
          StrategyFactory.createRandomActions())
        val board = Board.createRandomBoard(100, 100, 0.5f)
        val state = State(board, Play.initialRobotPosition)

        val numberOfTurns = 100
        val result = Play.execute(state, strategy, numberOfTurns)

        val worstPossibleScore =  numberOfTurns * Play.Scores.HitWall
        Console.println("End Score = " + result)
        assert(result >= worstPossibleScore)
      }
    }
  }
}
