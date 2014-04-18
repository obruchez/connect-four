package org.bruchez.connectfour.core

case class Game(redPlayer: Player, yellowPlayer: Player) {
  /**
   * Play a match between two players. Red player plays first.
   *
   * @return the sequence of board states and the game result (draw or red/yellow win)
   */
  def play: (Seq[Board], Result) = {
    // @tailrec
    def boardsAndResult(currentColor: Color,
                        currentBoard: Board,
                        previousBoards: Seq[Board]): (Seq[Board], Result) = {
      // Complete board history with current board
      val previousAndCurrentBoards = previousBoards :+ currentBoard

      // Determine the playing player from the current playing color (red/yellow), and invert the board if the yellow
      // player is playing (all players play from the same perspective - playing red)
      val (currentPlayer, currentBoardFromPlayerPerspective) =
        if (currentColor == Red) (redPlayer, currentBoard) else (yellowPlayer, currentBoard.withOppositeColors)

      // Determine the column to play
      val columnToPlay = currentPlayer.columnToPlay(currentBoardFromPlayerPerspective)

      // Next board state
      val nextBoard = currentBoard.withPiece(columnToPlay, currentColor)

      nextBoard.result match {
        case Some(result) =>
          // End of the game => next board is final board
          (previousAndCurrentBoards :+ nextBoard, result)
        case None =>
          // No result yet => keep going
          boardsAndResult(
            currentColor = currentColor.oppositeColor,
            currentBoard = nextBoard,
            previousBoards = previousAndCurrentBoards)
      }
    }

    boardsAndResult(currentColor = Red, currentBoard = Board.emptyBoard, previousBoards = Seq())
  }
}

object Game {
  // Number of pieces/discs to connect to win
  val ConnectedCountToWin = 4

  def main(args: Array[String]) {
    val redPlayer = RandomPlayer(id = "Random player", seed = 1234L)
    val yellowPlayer = LeftRightPlayer(id = "Left/right player", direction = LeftToRight)

    val (boards, result) = Game(redPlayer, yellowPlayer).play

    println(s"Result: $result\n")

    for ((board, index) <- boards.zipWithIndex) {
      println(s"Turn $index:\n\n$board\n")
    }
  }
}
