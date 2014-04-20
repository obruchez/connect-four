package org.bruchez.connectfour.core

sealed trait Result {
  def winningColor: Option[Color]
}

case object Draw extends Result {
  override val winningColor = None
  override val toString = "Draw"
}

case object RedWin extends Result {
  override val winningColor = Some(Red)
  override val toString = "Red win"
}

case object YellowWin extends Result {
  override val winningColor = Some(Yellow)
  override val toString = "Yellow win"
}

case class ResultWithBoardHistory(result: Result, boards: Seq[Board])
