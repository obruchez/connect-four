package org.bruchez.connectfour.core

import scala.util.Random

// Simple player playing randomly

case class RandomPlayer(override val id: String, seed: Long) extends Player {
  def columnToPlay(board: Board): Int = {
    val nonFullColumnIndexes = board.nonFullColumnIndexes.toIndexedSeq

    if (nonFullColumnIndexes.isEmpty) {
      throw new IllegalArgumentException
    } else {
      nonFullColumnIndexes(random.nextInt(nonFullColumnIndexes.size))
    }
  }

  private val random = new Random(seed)
}
