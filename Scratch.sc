
object Content extends Enumeration {
  type Content = Value
  val Empty, Can, Wall = Value
}

object Action extends Enumeration {
  type Action = Value
  val MoveNorth, MoveSouth, MoveEast, MoveWest, MoveRandom, Nothing, PickUpCan = Value
}

import Content._
import Action._

val x = Empty

val m = MoveNorth

case class Scenario(
              val north: Content,
              val south: Content,
              val east: Content,
              val west: Content,
              val current: Content
              ) {}

type Strategy = Map[Scenario, Action]

import util.Random.nextInt
def createRandomActions() = {
  Stream.continually(Action(nextInt(Action.maxId)))
}

val r1 = createRandomActions()
val r2 = createRandomActions()
r1(5)
r2(5)

val randomActions = Stream.continually(Action(nextInt(Action.maxId)))


val allScenarios =
  for {
    n <- Content.values
    s <- Content.values
    e <- Content.values
    w <- Content.values
    c <- Content.values
  } yield Scenario(n, s, e, w, c)

val ss = Scenario(Empty, Empty, Empty, Empty, Empty)

def myf(scenarios: Set[Scenario], actions: Stream[Action.Value]) : Strategy = {
  scenarios zip actions toMap
}

val randomStrategy = myf(allScenarios, randomActions)
val rs = randomStrategy(ss)
