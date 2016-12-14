package com.hombredequeso.robbierobot

object Board {

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
}
