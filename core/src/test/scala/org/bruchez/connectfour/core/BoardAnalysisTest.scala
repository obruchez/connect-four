package org.bruchez.connectfour.core

import org.specs2.mutable._

class BoardAnalysisTest extends Specification {
  "BoardAnalysis" should {
    "detect horizontal wins" in {
      val board0 = Board.emptyBoard
      board0.result must beEqualTo(None)

      val board1 = board0.withPiece(3, Red)
      board1.result must beEqualTo(None)

      val board2 = board1.withPiece(5, Red)
      board2.result must beEqualTo(None)

      val board3 = board2.withPiece(6, Red)
      board3.result must beEqualTo(None)

      val board4 = board3.withPiece(4, Red)
      board4.result must beEqualTo(Some(RedWin))
    }

    "detect vertical wins" in {
      val board0 = Board.emptyBoard
      board0.result must beEqualTo(None)

      val board1 = board0.withPiece(3, Yellow)
      board1.result must beEqualTo(None)

      val board2 = board1.withPiece(3, Yellow)
      board2.result must beEqualTo(None)

      val board3 = board2.withPiece(3, Yellow)
      board3.result must beEqualTo(None)

      val board4 = board3.withPiece(4, Red)
      board4.result must beEqualTo(None)

      val board5 = board4.withPiece(3, Yellow)
      board5.result must beEqualTo(Some(YellowWin))
    }

    "detect diagonal wins" in {
      val board0 = Board.emptyBoard
      board0.result must beEqualTo(None)

      val board1 = board0.withPiece(3, Yellow)
      board1.result must beEqualTo(None)

      val board2 = board1.withPiece(4, Red).withPiece(4, Yellow)
      board2.result must beEqualTo(None)

      val board3 = board2.withPiece(5, Red).withPiece(5, Red).withPiece(5, Yellow)
      board3.result must beEqualTo(None)

      val board4 = board3.withPiece(6, Red).withPiece(6, Red).withPiece(6, Red).withPiece(6, Yellow)
      board4.result must beEqualTo(Some(YellowWin))
    }
  }
}
