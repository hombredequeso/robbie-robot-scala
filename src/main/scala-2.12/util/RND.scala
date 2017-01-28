package com.hombredequeso.util

trait RandomProvider {
  def nextInt(): (Int, RandomProvider)
  def nextInt(n:Int): (Int, RandomProvider)
  def createRandomBool(oddsOfTrue: Float): Stream[Boolean]
}

object RND {

  def nextN[A]
  (count: Int)
  (getNext: RandomProvider => (A, RandomProvider))
  (rng: RandomProvider)
  : (List[A], RandomProvider) = {
    def go(count: Int, r: RandomProvider, xs: List[A]): (List[A], RandomProvider) =
      if (count == 0)
        (xs, r)
      else {
        val (x, r2) = getNext(r)
        go(count - 1, r2, x :: xs)
      }
    val result = go(count, rng, List())
    result
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

object RndState {
  def nextInt(size: Int)= State[RandomProvider, Int](_.nextInt(size))
  def nextInt = State[RandomProvider, Int](_.nextInt())
}
