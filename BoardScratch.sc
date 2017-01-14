def add(a: Int)(b: Int): Int = a + b
def doIt(t))


def generateNextPopulation
(getFitness: Float => Int)
(population: Vector[Float])
: Vector[Float] = {
  Vector[Float]()
}

val a = 0;



object Content extends Enumeration {
  val Empty, Can, Wall = Value
}

import util.Random.nextInt
def createRandomBool(oddsOfTrue: Float): Stream[Boolean] = {
  def getWeightedBool(oddsOfTrue: Float): Boolean = {
    val randomInt = (nextInt(100)) / 100f
    randomInt < oddsOfTrue
  }
  Stream.continually(getWeightedBool(oddsOfTrue))
}

type Board = Vector[Vector[Content.Value]]


def createRow(size: Int, hasCanStream: Stream[Boolean])
    : Stream[Content.Value] = {
  hasCanStream
    .take(size)
    .map(hasCan => if (hasCan) Content.Can else Content.Empty)
}

def createBoard(probs: Vector[Stream[Boolean]], width: Int)
    : Vector[Vector[Content.Value]] = {
  probs.map(r => createRow(width, r).toVector)
}

def createRandomBoard(width: Int, height: Int, hasCanProb: Float)
    : Vector[Vector[Content.Value]] = {
  val probs = Vector.fill(height)(createRandomBool(hasCanProb))
  createBoard(probs, width)
}

// val aa = createRandomBoard(10, 10, 0.023f)

// val canCount = aa
//                 .flatten
//                 .filter(c => c == Content.Can)
//                 .length

