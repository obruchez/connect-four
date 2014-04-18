package org.bruchez.connectfour.core

// A space in the board can be empty or contain either a red piece or a yellow piece

sealed trait Piece { def opponentPiece: Piece }

case object RedPiece extends Piece { override val opponentPiece = YellowPiece }
case object YellowPiece extends Piece { override val opponentPiece = RedPiece }

// A column in the board contains RowCount spaces which can either be empty (None) or contain a piece (Some(Piece))

case class Column(spaces: IndexedSeq[Option[Piece]]) {
  assert(spaces.size == Board.RowCount)

  def isEmpty: Boolean = !spaces.exists(_.isDefined)
  def isFull: Boolean = !spaces.exists(_.isEmpty)

  def firstEmptyRowIndex: Option[Int] = spaces.zipWithIndex.find(_._1.isEmpty).map(_._2)

  def withPiece(piece: Piece): Column =
    firstEmptyRowIndex match {
      case Some(index) => copy(spaces = spaces.updated(index, Some(piece)))
      case None => throw new IllegalArgumentException
    }
}

object Column {
  def emptyColumn: Column =
    Column(spaces = IndexedSeq.fill(Board.RowCount)(None))
}

// A board contains ColumnCount indexed columns

case class Board(columns: IndexedSeq[Column]) {
  assert(columns.size == Board.ColumnCount)

  def isEmpty: Boolean = !columns.exists(!_.isEmpty)
  def isFull: Boolean = !columns.exists(!_.isFull)

  def space(rowIndex: Int, columnIndex: Int): Option[Piece] = {
    assert(rowIndex >= 0 && rowIndex < Board.RowCount)
    assert(columnIndex >= 0 && columnIndex < Board.ColumnCount)

    columns(columnIndex).spaces(rowIndex)
  }

  def nonFullColumnIndexes: Set[Int] =
    columns.zipWithIndex.filterNot(_._1.isFull).map(_._2).toSet

  def withPiece(columnIndex: Int, piece: Piece): Board = {
    assert(nonFullColumnIndexes.contains(columnIndex))

    copy(columns = columns.updated(columnIndex, columns(columnIndex).withPiece(piece)))
  }
}

object Board {
  val ColumnCount = 7
  val RowCount = 6

  def emptyBoard: Board =
    Board(columns = IndexedSeq.fill(ColumnCount)(Column.emptyColumn))

  def main(args: Array[String]) {
    System.out.println("Hello, world!")
  }
}
