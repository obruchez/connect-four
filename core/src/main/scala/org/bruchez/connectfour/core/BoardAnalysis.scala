package org.bruchez.connectfour.core

trait BoardAnalysis {
  this: Board =>

  def result: Option[Result] =
    if (isFull) Some(Draw) else horizontalResult orElse verticalResult orElse diagonalResult

  private def horizontalResult: Option[Result] =
    (for {
      rowIndex <- 0 until Board.RowCount
      result <- this.result(startRow = rowIndex, startColumn = 0, rowStep = 0, columnStep = 1)
    } yield result).headOption

  private def verticalResult: Option[Result] =
    (for {
      columnIndex <- 0 until Board.ColumnCount
      result <- this.result(startRow = 0, startColumn = columnIndex, rowStep = 1, columnStep = 0)
    } yield result).headOption

  private def diagonalResult: Option[Result] =
    (for {
      rowIndex <- 0 until Board.RowCount
      result <- this.result(startRow = rowIndex, startColumn = 0, rowStep = 1, columnStep = 1) orElse
        this.result(startRow = rowIndex, startColumn = 0, rowStep = -1, columnStep = 1) orElse
        this.result(startRow = rowIndex, startColumn = Board.ColumnCount - 1, rowStep = 1, columnStep = -1) orElse
        this.result(startRow = rowIndex, startColumn = Board.ColumnCount - 1, rowStep = -1, columnStep = -1)
    } yield result).headOption

  /**
   * @return Result value along a horizontal, vertical, or diagonal trajectory in the board
   */
  private def result(startRow: Int,
                     startColumn: Int,
                     rowStep: Int,
                     columnStep: Int): Option[Result] = {
    var rowIndex = startRow
    var columnIndex = startColumn

    var colorOption: Option[Color] = None
    var sameColorCount = 0

    while (rowIndex >= 0 &&
      rowIndex < Board.RowCount &&
      columnIndex >= 0 &&
      columnIndex < Board.ColumnCount &&
      sameColorCount < Game.ConnectedCountToWin) {

      // Count consecutive pieces of the same color
      color(rowIndex, columnIndex) match {
        case Some(currentColor) =>
          if (colorOption == Some(currentColor)) {
            sameColorCount += 1
          } else {
            colorOption = Some(currentColor)
            sameColorCount = 1
          }
        case None =>
          colorOption = None
          sameColorCount = 0
      }

      rowIndex += rowStep
      columnIndex += columnStep
    }

    if (sameColorCount >= Game.ConnectedCountToWin) {
      // Enough connected pieces => win
      colorOption match {
        case Some(Red) => Some(RedWin)
        case Some(Yellow) => Some(YellowWin)
        case _ => None
      }
    } else {
      None
    }
  }
}
