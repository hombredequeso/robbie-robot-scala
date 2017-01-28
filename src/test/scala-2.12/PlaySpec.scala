package com.hombredequeso.robbierobot

import com.hombredequeso.robbierobot.Board.Board
import com.hombredequeso.robbierobot.Play.{Coord, PlayStrategy, State}
import com.hombredequeso.util.RND.ScalaRandomizer
import org.scalatest.{FunSpec, Matchers}

class PlaySpec extends FunSpec with Matchers{

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

  describe("outsideBoardLimit") {

    it("returns false if position is on the board") {
      val xSize = 10
      val ySize = 15

      val board = Board.createSingleContentBoard(xSize, ySize)(Content.Empty)

      Play.outsideBoardLimit(board, Coord(0,0)) shouldBe false
      Play.outsideBoardLimit(board, Coord(1,0)) shouldBe false
      Play.outsideBoardLimit(board, Coord(0,1)) shouldBe false
      Play.outsideBoardLimit(board, Coord(xSize-1,0)) shouldBe false
      Play.outsideBoardLimit(board, Coord(0,ySize -1)) shouldBe false

      Play.outsideBoardLimit(board, Coord(xSize,0)) shouldBe true
      Play.outsideBoardLimit(board, Coord(0,ySize)) shouldBe true
    }
  }
}
