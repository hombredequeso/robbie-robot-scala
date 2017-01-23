package com.hombredequeso.util

trait RandomProvider {
  def nextInt(): Int
  def nextInt(seed:Int): Int
  def createRandomBool(oddsOfTrue: Float): Stream[Boolean]
}

object RND {
  class ScalaRandomizer extends RandomProvider{
    val random = new scala.util.Random()

    def createRandomBool(oddsOfTrue: Float): Stream[Boolean] = {
      def getWeightedBool(oddsOfTrue: Float): Boolean = {
        val randomInt = (random.nextInt(100)) / 100f
        randomInt < oddsOfTrue
      }

      Stream.continually(getWeightedBool(oddsOfTrue))
    }

    def nextInt() = {
      random.nextInt()
    }
    def nextInt(seed:Int) = {
      random.nextInt(seed)
    }
  }
}
