package hello

import javafx.beans.property.SimpleBooleanProperty

import com.hombredequeso.geneticAlgorithm.Optimizer
import com.hombredequeso.robbierobot.Play.{PlayStrategy, State}
import com.hombredequeso.robbierobot.Strategy.StrategyMap
import com.hombredequeso.robbierobot._

import scala.concurrent.{ExecutionContext, Future}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.Label
import scalafx.scene.shape._
import scalafx.animation._
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scala.util.Random


object AlgorithmRunner {

  val iterationCount = 20
  val populationSize = 200
  val boardCount = 100
  val numberOfTurnsPerBoard = 200

  def execute(): StrategyMap ={
    val initialPopulation = StrategyFactory.createInitialPopulation(populationSize)
    val getFitness = Strategy.getStrategyFitness(boardCount, numberOfTurnsPerBoard)_
    val result: StrategyMap = Optimizer.findOptimalStrategy(Evolve.generateNextPopulation)(getFitness)(iterationCount)(initialPopulation)
    result
  }
}

object GamePlay {
  var board = Board.createRandomBoard(10, 10, 0.5f).map(x => x.toList).toList
  var robotPos = Play.initialRobotPosition
  val r = new Random()

  def executeTurn(): Unit = {
    val startState = State(board, robotPos)
    val playStrategy = PlayStrategy(HelloSBT.strategy.get, r)
    val (newState, _) = Play.executeTurn(playStrategy)(startState)
    board = newState.board.map(x => x.toList).toList
    robotPos = newState.robotPos
  }
}

object HelloSBT extends JFXApp {
  val offset: Double = 10.0
  var lastTime: Long = 0
  val rate: Long = 100000000

  val xWindowWidth = 1000
  val yWindowHeight = 1000


  implicit val ec = ExecutionContext.global
  var strategy: Option[StrategyMap] = None
  val futureResult = Future[Boolean] {
    val bestStrategy = AlgorithmRunner.execute()
    strategy = Some(bestStrategy)
    true
  }

  /*
  var strategy: Option[StrategyMap] = Some(StrategyFactory.initStrategy(Action.MoveRandom))
    */

  stage = new PrimaryStage {
    title = "Robbie the Rubbish Robot"
    scene = new Scene(xWindowWidth,yWindowHeight) {

      var model = createBoard(10, 10)
      val circle = createRobot()
      val viewModel = createGrid(10,10, xWindowWidth,yWindowHeight, model)
      content = viewModel :+ circle

      val timer = AnimationTimer((t:Long) => {
        if (((t - lastTime) > rate)&& strategy.isDefined) {
          lastTime = t
          GamePlay.executeTurn()
          updateModel(GamePlay.board, model)
          updateRobotPos(GamePlay.robotPos, circle)
        }
      })
      timer.start
    }
  }

  def createRobot() = {
    Circle(50, 50, 50)
  }

  def updateModel(
                   board: List[List[Content.Value]],
                   model: Vector[Vector[SimpleBooleanProperty]]): Unit = {
    val b = board.map(_.toVector).toVector
    val xTotal = b.length
    val yTotal = b(0).length
    for (
      x <- 0 to xTotal-1;
      y <- 0 to yTotal-1
    )
      model(x)(y).set(board(x)(y) == Content.Can)
  }

  def updateRobotPos(robotPos: Play.Coord, circle: Circle)= {
    circle.centerX = (robotPos.x * 100) + 50
    circle.centerY = (robotPos.y * 100) + 50
  }

  def createBoard(xCount: Int, yCount: Int) :Vector[Vector[SimpleBooleanProperty]]= {
    (0 to xCount)
      .map(x => (0 to yCount)
        .map(y => new SimpleBooleanProperty(false)).toVector)
      .toVector
  }

  def createGrid(
                  xCount : Int, yCount: Int,
                  totalX: Int, totalY: Int,
                  board: Vector[Vector[SimpleBooleanProperty]]
                ): List[Rectangle] = {
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

