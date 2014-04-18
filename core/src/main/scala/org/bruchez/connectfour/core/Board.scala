package org.bruchez.connectfour.core

// A space in the board can be empty or contain either a red piece or a yellow piece

sealed trait Color {
  def oppositeColor: Color
}

case object Red extends Color {
  override val oppositeColor = Yellow
  override val toString = "X"
}
case object Yellow extends Color {
  override val oppositeColor = Red
  override val toString = "O"
}

// A column in the board contains RowCount spaces which can either be empty (None) or contain a piece (Some(Color))

case class Column(colors: IndexedSeq[Option[Color]]) {
  assert(colors.size == Board.RowCount)

  def isEmpty: Boolean = !colors.exists(_.isDefined)
  def isFull: Boolean = !colors.exists(_.isEmpty)

  def firstEmptyRowIndex: Option[Int] = colors.zipWithIndex.find(_._1.isEmpty).map(_._2)

  def withPiece(color: Color): Column = firstEmptyRowIndex match {
    case Some(index) => copy(colors = colors.updated(index, Some(color)))
    case None => throw new IllegalArgumentException
  }

  def withOppositeColors: Column =
    copy(colors = colors.map(_.map(_.oppositeColor)))
}

object Column {
  def emptyColumn: Column =
    Column(colors = IndexedSeq.fill(Board.RowCount)(None))
}

// A board contains ColumnCount indexed columns

case class Board(columns: IndexedSeq[Column]) extends BoardAnalysis {
  assert(columns.size == Board.ColumnCount)

  def isEmpty: Boolean = !columns.exists(!_.isEmpty)
  def isFull: Boolean = !columns.exists(!_.isFull)

  def color(rowIndex: Int, columnIndex: Int): Option[Color] = {
    assert(rowIndex >= 0 && rowIndex < Board.RowCount)
    assert(columnIndex >= 0 && columnIndex < Board.ColumnCount)

    columns(columnIndex).colors(rowIndex)
  }

  def nonFullColumnIndexes: Set[Int] =
    columns.zipWithIndex.filterNot(_._1.isFull).map(_._2).toSet

  def withPiece(columnIndex: Int, color: Color): Board = {
    assert(nonFullColumnIndexes.contains(columnIndex))

    copy(columns = columns.updated(columnIndex, columns(columnIndex).withPiece(color)))
  }

  def withOppositeColors: Board =
    copy(columns = columns.map(_.withOppositeColors))

  override def toString: String =
    asStrings.mkString("\n")

  def asStrings: Seq[String] = {
    val rowSeparator = "-" * (Board.ColumnCount * 2 + 1)

    (for (rowIndex <- Board.RowCount - 1 to 0 by -1) yield {
      val row = (for (columnIndex <- 0 until Board.ColumnCount)
        yield "|" + color(rowIndex, columnIndex).map(_.toString).getOrElse(" ")).mkString + "|"

      Seq(rowSeparator, row)
    }).flatten :+ rowSeparator
  }
}

object Board {
  // Size of the board/grid
  val ColumnCount = 7
  val RowCount = 6

  def emptyBoard: Board =
    Board(columns = IndexedSeq.fill(ColumnCount)(Column.emptyColumn))
}
