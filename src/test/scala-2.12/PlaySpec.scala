package com.hombredequeso.robbierobot

import com.hombredequeso.robbierobot.Play.{PlayStrategy, State}
import com.hombredequeso.util.RND.ScalaRandomizer
import org.scalatest.FunSpec

class PlaySpec extends FunSpec {

  describe("executeTurn") {
    describe("with random input data") {
      it("should return new state and turn score which is higher than lowest possible score") {
        var randomizer = new ScalaRandomizer()
        val strategy = StrategyFactory.make(
          StrategyFactory.allScenarios,
          StrategyFactory.createRandomActions(randomizer))
        val board = Board.createRandomBoard(randomizer)(10, 10, 0.5f)
        val state = State(board, Play.initialRobotPosition)
        val result = Play.executeTurn(PlayStrategy(strategy, new ScalaRandomizer()))(state)
        assert(result._2 >= Play.Scores.HitWall)
      }
    }
  }

  describe("execute") {
    describe("with random input data") {
      val numberOfTurns = 4
      it(s"should return higher than theoretical minimum score (number of turns = ${numberOfTurns})") {
        val randomizer = new ScalaRandomizer()
        val strategy = StrategyFactory.make(
          StrategyFactory.allScenarios,
          StrategyFactory.createRandomActions(randomizer))
        val board = Board.createRandomBoard(randomizer)(10, 10, 0.5f)
        val state = State(board, Play.initialRobotPosition)

        val numberOfTurns = 100
        val r = new ScalaRandomizer()
        val result = Play.execute(r)(state, strategy, numberOfTurns)

        val worstPossibleScore =  numberOfTurns * Play.Scores.HitWall
        assert(result >= worstPossibleScore)
      }
    }
  }
}
