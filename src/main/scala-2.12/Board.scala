package com.hombredequeso.robbierobot

import com.hombredequeso.util.RND.ScalaRandomizer

object Board {

  type Board = Seq[Seq[Content.Value]]

  def createRow(size: Int, hasCanStream: Stream[Boolean])
  : Stream[Content.Value] = {
    hasCanStream
      .take(size)
      .map(hasCan => if (hasCan) Content.Can else Content.Empty)
  }

  def createBoard(probs: Seq[Stream[Boolean]], width: Int)
  : Seq[Seq[Content.Value]] = {
    probs.map(r => createRow(width, r))
  }

  def createRandomBoard(width: Int, height: Int, hasCanProb: Float)
  : Seq[Seq[Content.Value]] = {
    var randomizer = new ScalaRandomizer()
    val probs = Seq.fill(height)(randomizer.createRandomBool(hasCanProb))
    createBoard(probs, width)
  }
}
