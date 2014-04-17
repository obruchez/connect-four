package org.bruchez.connectfour.core

sealed trait Space

case object Empty extends Space
case object Red extends Space
case object Yellow extends Space

case class Column(spaces: Map[Int, Space])

case class Grid(columns: Map[Int, Column])

object Grid {
  val ColumnCount = 7
  val RowCount = 6

  def main(args: Array[String]) {
    System.out.println("Hello, world!")
  }
}
