package org.bruchez.connectfour.core

case class Game(redPlayer: Player, yellowPlayer: Player) {
  /**
   * Play a match between two players. Red player plays first.
   *
   * @return the sequence of board states and the game result (draw or red/yellow win)
   */
  def play: ResultWithBoardHistory = {
    // @tailrec
    def boardsAndResult(currentColor: Color,
                        currentBoard: Board,
                        previousBoards: Seq[Board]): ResultWithBoardHistory = {
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
          ResultWithBoardHistory(result = result, boards = previousAndCurrentBoards :+ nextBoard)
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
    //System.setProperty("java.library.path", "/opt/local/lib/")

    val redPlayer = RandomPlayer.randomPlayer(id = "Random player")
    //val yellowPlayer2 = LeftRightPlayer.randomPlayer(id = "Left/right player")
    val yellowPlayer = NeuralPlayer.randomPlayer(id = "Neural player")

    val resultWithBoardHistory = Game(redPlayer, yellowPlayer).play

    println(s"Result: ${resultWithBoardHistory.result}\n")

    for ((board, index) <- resultWithBoardHistory.boards.zipWithIndex) {
      println(s"Turn $index:\n\n$board\n")
    }

    redPlayer.learn(playerColor = Red, resultWithBoardHistory = resultWithBoardHistory)
    yellowPlayer.learn(playerColor = Yellow, resultWithBoardHistory = resultWithBoardHistory)
  }
}
