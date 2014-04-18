package org.bruchez.connectfour.core

// A space in the grid can be empty or contain either a red piece or a yellow piece

sealed trait Piece

case object RedPiece extends Piece
case object YellowPiece extends Piece

// A column in the grid contains Grid.RowCount spaces which can either be empty (None) or contain a piece (Some(Piece))

case class Column(spaces: IndexedSeq[Option[Piece]]) {
  assert(spaces.size == Grid.RowCount)

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
  val EmptyColumn = Column(spaces = IndexedSeq.fill(Grid.RowCount)(None))
}

// A grid contains Grid.ColumnCount indexed columns

case class Grid(columns: IndexedSeq[Column]) {
  assert(columns.size == Grid.ColumnCount)

  def isEmpty: Boolean = !columns.exists(!_.isEmpty)
  def isFull: Boolean = !columns.exists(!_.isFull)

  def space(rowIndex: Int, columnIndex: Int): Option[Piece] = {
    assert(rowIndex >= 0 && rowIndex < Grid.RowCount)
    assert(columnIndex >= 0 && columnIndex < Grid.ColumnCount)

    columns(columnIndex).spaces(rowIndex)
  }

  def nonFullColumnIndexes: Set[Int] =
    columns.zipWithIndex.filterNot(_._1.isFull).map(_._2).toSet

  def withPiece(columnIndex: Int, piece: Piece): Grid = {
    assert(nonFullColumnIndexes.contains(columnIndex))

    copy(columns = columns.updated(columnIndex, columns(columnIndex).withPiece(piece)))
  }
}

object Grid {
  val ColumnCount = 7
  val RowCount = 6

  val EmptyGrid = Grid(columns = IndexedSeq.fill(Grid.ColumnCount)(Column.EmptyColumn))

  def main(args: Array[String]) {
    System.out.println("Hello, world!")
  }
}
