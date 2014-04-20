package org.bruchez.connectfour.core

// Player using a neural network to play

case class NeuralPlayer(override val id: String, lambda: Double) extends Player {
  override def columnToPlay(board: Board): Int = {
    val nonFullColumnIndexes = board.nonFullColumnIndexes.toIndexedSeq

    if (nonFullColumnIndexes.isEmpty) {
      throw new IllegalArgumentException
    } else {
      // @todo temporary algorithm
      val index = nonFullColumnIndexes(temporaryIndexChoice % nonFullColumnIndexes.size)
      temporaryIndexChoice += 1
      index
    }
  }

  // @todo temporary
  private var temporaryIndexChoice = 0

  override def learn(playerColor: Color, resultWithBoardHistory: ResultWithBoardHistory): NeuralPlayer = {
    val trainingData = NeuralPlayer.trainingData(resultWithBoardHistory, lambda)

    // @todo make the neural network learn from the training data

    this
  }
}

object NeuralPlayer {
  val InputsPerSpace = 3

  // Serialize a board to a sequence of neural network inputs (i.e. sequence of [0.0, 1.0] values)
  def inputsFromBoard(board: Board): Seq[Double] = (for {
    rowIndex <- 0 until Board.RowCount
    columnIndex <- 0 until Board.ColumnCount
    color = board.color(rowIndex, columnIndex)
  } yield color match {
    case None =>
      Seq(1.0, 0.0, 0.0)
    case Some(Red) =>
      Seq(0.0, 1.0, 0.0)
    case Some(Yellow) =>
      Seq(0.0, 0.0, 1.0)
  }).flatten

  def trainingData(resultWithBoardHistory: ResultWithBoardHistory, lambda: Double): Seq[TrainingData] = {
    val boardsFromEndToStart = resultWithBoardHistory.boards.reverse
    lazy val boardsWithOppositeColors = boardsFromEndToStart.map(_.withOppositeColors)

    // Main perspective = red player => boards must be inverted to get the yellow perspective
    resultWithBoardHistory.result match {
      case Draw =>
        trainingData(referenceOutput = 0.5, boardsFromEndToStart, lambda)
      case RedWin =>
        // Learn from the red (win -> 1.0) and yellow (defeat -> 0.0)
        trainingData(referenceOutput = 1.0, boardsFromEndToStart, lambda) ++
          trainingData(referenceOutput = 0.0, boardsWithOppositeColors, lambda)
      case YellowWin =>
        // Learn from the red (defeat -> 0.0) and yellow (win -> 1.0)
        trainingData(referenceOutput = 0.0, boardsFromEndToStart, lambda) ++
          trainingData(referenceOutput = 1.0, boardsWithOppositeColors, lambda)
    }
  }

  private def trainingData(referenceOutput: Double, boards: Seq[Board], lambda: Double): Seq[TrainingData] = {
    val delta = referenceOutput - 0.5

    // The first board must be associated with output = 0.5 + delta
    // The second board must be associated with output = 0.5 + delta * lambda
    // The third board must be associated with output = 0.5 + delta * lambda * lambda
    // etc.
    //
    // => general formula: output = 0.5 + delta * pow(lambda, index), where index is the zero-based index of the board

    for {
      (board, index) <- boards.zipWithIndex
      inputs = inputsFromBoard(board)
      output = 0.5 + delta * math.pow(lambda, index.toDouble)
    } yield TrainingData(inputs = inputs, output = output)
  }
}

// Inputs and output used for training the neural network
case class TrainingData(inputs: Seq[Double], output: Double) {
  assert(inputs.size == Board.RowCount * Board.ColumnCount * NeuralPlayer.InputsPerSpace)
}
