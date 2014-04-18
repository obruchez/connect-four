package org.bruchez.connectfour.core

trait Player {
  def id: String

  /**
   * @param board current, non-full board
   * @return column to play according to the player
   * @note in the context of this method, all players play the red color (i.e. the board will be modified accordingly
   *       before columnToPlay is called, if needed)
   */
  def columnToPlay(board: Board): Int
}
