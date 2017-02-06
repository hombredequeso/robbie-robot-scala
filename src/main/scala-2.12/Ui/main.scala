package com.hombredequeso.robbierobot.main

import com.hombredequeso.robbierobot.Evolve.{GenerationContext, GenerationStatistics}
import com.hombredequeso.robbierobot.Ui.{AlgorithmRunner, GamePlay, View, Vm}
import com.hombredequeso.robbierobot.Play
import com.hombredequeso.robbierobot.Strategy.StrategyMap
import com.hombredequeso.util.RND.ScalaRandomizer

import scala.concurrent.{ExecutionContext, Future}
import scalafx.animation._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.Includes._

object PlayParameters {
  val xSquareCount = 10
  val ySquareCount = 10
}

object StatWriter {
  def DumpToConsole(stats: List[String]): Unit = {
  Console.println("Start: statWriter.stats =============================================")
  stats.foreach(Console.println(_))
  Console.println("End: statWriter.stats =============================================")
  }
}


object AppMain extends JFXApp {
  var lastTime: Long = 0
  val rate: Long = 300000000

  val xWindowWidth = 1000
  val yWindowHeight = 1000

  implicit val ec = ExecutionContext.global
  var strategy: Option[StrategyMap] = None
  var gotStrategy = Future[Boolean] {

    val generationContext = new GenerationContext(
      new ScalaRandomizer(),
      new GenerationStatistics(List(), 0))

    val bestStrategyWithState = AlgorithmRunner.execute().run(generationContext)
    val bestStrategy = bestStrategyWithState._1
    Console.println(s"Final fitness: ${bestStrategy._2}")
    strategy = Some(bestStrategy._1)
    StatWriter.DumpToConsole(bestStrategyWithState._2.statWriter.getStats())
    true
  }

  val randomizer = new ScalaRandomizer()
  def initializeGame() = {
    GamePlay.InitializeGameState(
      randomizer)(
      PlayParameters.xSquareCount,
      PlayParameters.ySquareCount,
      Play.initialRobotPosition
    )
  }

  initializeGame()

  stage = new PrimaryStage {
    title = "Robbie the Rubbish Robot"
    scene = new Scene(xWindowWidth,yWindowHeight) {
      content = View.getContent(Vm.viewModel, xWindowWidth,yWindowHeight)
      onMouseClicked = handle {
        initializeGame()
      }

      val timer = AnimationTimer((t:Long) => {
        if (((t - lastTime) > rate)&& strategy.isDefined) {
          lastTime = t
          GamePlay.executeTurn(strategy.get)
          Vm.updateViewModel(GamePlay.gameState, Vm.viewModel)
        }
      })
      timer.start
    }
  }
}
