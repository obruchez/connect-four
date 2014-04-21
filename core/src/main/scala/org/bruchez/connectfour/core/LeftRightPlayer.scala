package org.bruchez.connectfour.core

import scala.util.Random

sealed trait Direction

case object LeftToRight extends Direction
case object RightToLeft extends Direction

// Simple player playing systematically from left to right or from right to left

case class LeftRightPlayer(override val id: String, direction: Direction) extends Player {
  override def columnToPlay(board: Board): Int = {
    val nonFullColumnIndexes = board.nonFullColumnIndexes

    if (nonFullColumnIndexes.isEmpty) {
      throw new IllegalArgumentException
    } else if (direction == LeftToRight) {
      nonFullColumnIndexes.min
    } else {
      // direction == RightToLeft
      nonFullColumnIndexes.max
    }
  }

  override def learn(playerColor: Color, resultWithBoardHistory: ResultWithBoardHistory): LeftRightPlayer = {
    // This player doesn't learn...
    this
  }
}

object LeftRightPlayer extends PlayerGeneratorAndSerializer[LeftRightPlayer] {
  override def randomPlayer(id: String): LeftRightPlayer = {
    val direction = if (random.nextBoolean()) LeftToRight else RightToLeft

    LeftRightPlayer(id, direction)
  }

  private val random = new Random
}
