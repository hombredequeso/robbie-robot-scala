package com.hombredequeso.robbierobot.Ui

import scalafx.beans.property.DoubleProperty
import javafx.beans.property.SimpleBooleanProperty

import com.hombredequeso.robbierobot.{Content, Play}
import com.hombredequeso.robbierobot.main.PlayParameters._

object Vm {

  val gridViewModel = createGridViewModel(xSquareCount, ySquareCount)
  val robotViewModel = RobotViewModel(DoubleProperty(0), DoubleProperty(2))
  val viewModel = ViewModel(gridViewModel, robotViewModel)

  case class RobotViewModel(
                             val xPos: DoubleProperty,
                             val yPos: DoubleProperty) {}

  private def createGridViewModel(xCount: Int, yCount: Int) :Vector[Vector[SimpleBooleanProperty]]= {
    (0 to xCount-1)
      .map(x => (0 to yCount-1)
        .map(y => new SimpleBooleanProperty(false)).toVector)
      .toVector
  }

  case class ViewModel(
                        val grid: Vector[Vector[SimpleBooleanProperty]],
                        val robot: RobotViewModel)

  def updateViewModel( gameState: Play.State, viewModel: ViewModel) = {
    updateGridViewModel(gameState.board, viewModel.grid)
    updateRobotViewModel(gameState.robotPos, viewModel.robot)
  }

  private def updateGridViewModel(
                                   board: Seq[Seq[Content.Value]],
                                   viewModel: Vector[Vector[SimpleBooleanProperty]]): Unit = {
    val b = board.map(_.toVector).toVector
    val xTotal = b.length
    val yTotal = b(0).length
    for (
      x <- 0 to xTotal-1;
      y <- 0 to yTotal-1
    )
      viewModel(x)(y).set(board(x)(y) == Content.Can)
  }

  private def updateRobotViewModel(robotPos: Play.Coord, viewModel: RobotViewModel): Unit = {
    viewModel.xPos.value = (robotPos.x * 100) + 50
    viewModel.yPos.value = (robotPos.y * 100) + 50
  }
}

