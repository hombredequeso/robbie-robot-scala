package com.hombredequeso.util

trait RandomProvider {
  def nextInt(): (Int, RandomProvider)
  def nextInt(n:Int): (Int, RandomProvider)
  def createRandomBool(oddsOfTrue: Float): Stream[Boolean]
}

object RND {

  // tail-recursively get a List[Int] from a RandomProvider
  // largely copied from Chiusano
  def nextInts(count: Int)(rng: RandomProvider): (List[Int], RandomProvider) = {
    def go(count: Int, r: RandomProvider, xs: List[Int]): (List[Int], RandomProvider) =
      if (count == 0)
        (xs, r)
      else {
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
      }
    go(count, rng, List())
  }

  class ScalaRandomizer(val random: scala.util.Random) extends RandomProvider{
    def this(seed: Long) {
      this(new scala.util.Random(seed))
    }
    def this() {
      this(new scala.util.Random())
    }

    def createRandomBool(oddsOfTrue: Float): Stream[Boolean] = {
      def getWeightedBool(oddsOfTrue: Float): Boolean = {
        val randomInt = (random.nextInt(100)) / 100f
        randomInt < oddsOfTrue
      }
      Stream.continually(getWeightedBool(oddsOfTrue))
    }

    def nextInt(): (Int, RandomProvider) = {
      (random.nextInt(), this)
    }
    def nextInt(n:Int): (Int, RandomProvider) = {
      (random.nextInt(n), this)
    }
  }
}
