package Ui

import javafx.beans.property.SimpleBooleanProperty

import Vm.{RobotViewModel, ViewModel}

import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Rectangle, Shape}

import scalafx.Includes._

object View {

  def getContent(
                  viewModel: ViewModel,
                  xWindowWidth: Int,
                  yWindowHeight: Int)
  : List[Shape] = {
    val robotView = createRobotView(viewModel.robot)
    val gridView = createGridView(xWindowWidth,yWindowHeight, viewModel.grid)
    gridView :+ robotView
  }

  private def createRobotView(vm: RobotViewModel) = {
    new Circle {
      centerX <== vm.xPos
      centerY <== vm.yPos
      radius = 50
    }
  }

  private def createGridView(
                              totalX: Int, totalY: Int,
                              board: Vector[Vector[SimpleBooleanProperty]]
                            ): List[Rectangle] = {
    val xCount = board.length
    val yCount = board(0).length
    val xSize = totalX.toDouble / xCount.toDouble
    val ySize = totalY.toDouble / yCount.toDouble
    val all = for(
      x <- 0 to xCount-1;
      y <- 0 to yCount-1
    ) yield (x,y)

    all.map(a => {
      val (x1, y1):(Int, Int) = a
      val rectX = (x1 * xSize)
      val rectY = (y1 * ySize)
      new Rectangle {
        x = rectX
        y = rectY
        width = xSize
        height = ySize
        stroke = Color.Black
        fill <== when(board(x1)(y1)) choose Color.Brown otherwise Color.White
      }
    }).toList
  }
}

