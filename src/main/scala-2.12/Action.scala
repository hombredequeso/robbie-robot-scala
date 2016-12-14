package com.hombredequeso.robbierobot

object Action extends Enumeration {
  type Action = Value
  val MoveNorth, MoveSouth, MoveEast, MoveWest, MoveRandom, Nothing, PickUpCan = Value
}
