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

  /**
   * Learn from a finished game (i.e. match) between two players.
   *
   * @param playerColor color used by this player
   * @param resultWithBoardHistory game result and sequence of boards from start of game (empty board) to end of game
   *                               (draw, red win, or yellow win)
   * @note the boards in the context of this method are *not* inverted
   * @return a new instance of the player after having learned
   */
  def learn(playerColor: Color, resultWithBoardHistory: ResultWithBoardHistory): Player
}
