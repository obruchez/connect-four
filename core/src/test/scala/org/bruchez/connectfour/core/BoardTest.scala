package org.bruchez.connectfour.core

import org.specs2.mutable._

class ColumnTest extends Specification {
  "Column" should {
    "behave consistently when completely empty" in {
      val emptyColumn = Column.emptyColumn

      emptyColumn.isEmpty must beTrue
      emptyColumn.isFull must beFalse

      emptyColumn.firstEmptyRowIndex must beEqualTo(Some(0))
    }

    "behave consistently when not empty nor full" in {
      val nonEmptyColumn = Column.emptyColumn.withPiece(RedPiece)

      nonEmptyColumn.isEmpty must beFalse
      nonEmptyColumn.isFull must beFalse

      nonEmptyColumn.firstEmptyRowIndex must beEqualTo(Some(1))
    }

    "behave consistently when completely full" in {
      val fullColumn = ColumnTest.fullColumn(firstPiece = RedPiece)

      fullColumn.isEmpty must beFalse
      fullColumn.isFull must beTrue

      fullColumn.firstEmptyRowIndex must beEqualTo(None)
    }
  }
}

object ColumnTest {
  def fullColumn(firstPiece: Piece): Column = {
    // @tailrec
    def fullColumn(pieceToAdd: Piece, column: Column): Column =
      if (column.isFull) column else fullColumn(pieceToAdd.opponentPiece, column.withPiece(pieceToAdd))

    fullColumn(firstPiece, Column.emptyColumn)
  }
}

class BoardTest extends Specification {
  "Board" should {
    "behave consistently when completely empty" in {
      val emptyBoard = Board.emptyBoard

      emptyBoard.isEmpty must beTrue
      emptyBoard.isFull must beFalse

      for (rowIndex <- 0 until Board.RowCount; columnIndex <- 0 until Board.ColumnCount) {
        emptyBoard.space(rowIndex, columnIndex).isEmpty must beTrue
      }

      emptyBoard.nonFullColumnIndexes.size must beEqualTo(Board.ColumnCount)
    }

    "behave consistently when not empty nor full" in {
      val nonEmptyBoard = Board.emptyBoard.withPiece(columnIndex = 4, piece = YellowPiece)

      nonEmptyBoard.isEmpty must beFalse
      nonEmptyBoard.isFull must beFalse

      nonEmptyBoard.nonFullColumnIndexes.size must beEqualTo(Board.ColumnCount)

      val oneFullColumnBoard =
        nonEmptyBoard.copy(columns = nonEmptyBoard.columns.updated(4, ColumnTest.fullColumn(YellowPiece)))

      oneFullColumnBoard.isEmpty must beFalse
      oneFullColumnBoard.isFull must beFalse

      oneFullColumnBoard.nonFullColumnIndexes.size must beEqualTo(Board.ColumnCount - 1)
    }

    "behave consistently when completely full" in {
      val fullBoard = BoardTest.fullBoard(firstPiece = RedPiece)

      fullBoard.isEmpty must beFalse
      fullBoard.isFull must beTrue

      for (rowIndex <- 0 until Board.RowCount; columnIndex <- 0 until Board.ColumnCount) {
        fullBoard.space(rowIndex, columnIndex).isEmpty must beFalse
      }

      fullBoard.nonFullColumnIndexes.size must beEqualTo(0)
    }
  }
}

object BoardTest {
  def fullBoard(firstPiece: Piece): Board = {
    val firstColumn = ColumnTest.fullColumn(firstPiece)
    val secondColumn = ColumnTest.fullColumn(firstPiece.opponentPiece)

    assert(Board.ColumnCount % 2 == 1)

    Board(columns = IndexedSeq.fill(Board.ColumnCount / 2)(IndexedSeq(firstColumn, secondColumn)).flatten :+ firstColumn)
  }
}
