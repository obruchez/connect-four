package org.bruchez.connectfour.core

import scala.util.Random

// Simple player playing randomly

case class RandomPlayer(override val id: String, seed: Long) extends Player {
  override def columnToPlay(board: Board): Int = {
    val nonFullColumnIndexes = board.nonFullColumnIndexes.toIndexedSeq

    if (nonFullColumnIndexes.isEmpty) {
      throw new IllegalArgumentException
    } else {
      nonFullColumnIndexes(random.nextInt(nonFullColumnIndexes.size))
    }
  }

  override def learn(playerColor: Color, resultWithBoardHistory: ResultWithBoardHistory): RandomPlayer = {
    // This player doesn't learn...
    this
  }

  private val random = new Random(seed)
}
