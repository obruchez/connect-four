package org.bruchez.connectfour.core

import breeze.linalg.Matrix
import com.github.neuralnetworks.architecture.NeuralNetworkImpl
import com.github.neuralnetworks.architecture.types.NNFactory

// Player using a neural network to play

case class NeuralPlayer(override val id: String,
                        weights: Seq[Matrix[Float]],
                        temporalDifferenceLambda: Float,
                        learningRate: Float,
                        momentum: Float,
                        l1weightDecay: Float,
                        l2weightDecay: Float) extends Player {
  assert(weights.size >= 1)
  assert(weights.head.cols == NeuralPlayer.InputCount)
  assert(weights.last.rows == NeuralPlayer.OutputCount)

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
    val trainingData = NeuralPlayer.trainingData(resultWithBoardHistory, temporalDifferenceLambda)

    // @todo make the neural network learn from the training data

    this
  }

  private val neuralNetwork = NeuralPlayer.neuralNetworkFromWeights(weights)
}

object NeuralPlayer extends PlayerGeneratorAndSerializer[NeuralPlayer] {
  private val InputsPerSpace = 3

  val InputCount = Board.RowCount * Board.ColumnCount * NeuralPlayer.InputsPerSpace
  val OutputCount = 1

  override def randomPlayer(id: String): NeuralPlayer = {
    // @todo randomize weights and learning parameters

    val weights: Seq[Matrix[Float]] = Seq(Matrix.zeros(OutputCount, InputCount))

    NeuralPlayer(id = id,
      weights = weights,
      temporalDifferenceLambda = 0.9f,
      learningRate = 1.0f,
      momentum = 0.0f,
      l1weightDecay = 0.0f,
      l2weightDecay = 0.0f)
  }

  // Serialize a board to a sequence of neural network inputs (i.e. sequence of [0.0, 1.0] values)
  private def inputsFromBoard(board: Board): Seq[Float] = (for {
    rowIndex <- 0 until Board.RowCount
    columnIndex <- 0 until Board.ColumnCount
    color = board.color(rowIndex, columnIndex)
  } yield color match {
    case None =>
      Seq(1.0f, 0.0f, 0.0f)
    case Some(Red) =>
      Seq(0.0f, 1.0f, 0.0f)
    case Some(Yellow) =>
      Seq(0.0f, 0.0f, 1.0f)
  }).flatten

  private def trainingData(resultWithBoardHistory: ResultWithBoardHistory,
                           temporalDifferenceLambda: Float): Seq[TrainingData] = {
    val boardsFromEndToStart = resultWithBoardHistory.boards.reverse
    lazy val boardsWithOppositeColors = boardsFromEndToStart.map(_.withOppositeColors)

    // Main perspective = red player => boards must be inverted to get the yellow perspective
    resultWithBoardHistory.result match {
      case Draw =>
        trainingData(referenceOutput = 0.5f, boardsFromEndToStart, temporalDifferenceLambda)
      case RedWin =>
        // Learn from the red (win -> 1.0) and yellow (defeat -> 0.0)
        trainingData(referenceOutput = 1.0f, boardsFromEndToStart, temporalDifferenceLambda) ++
          trainingData(referenceOutput = 0.0f, boardsWithOppositeColors, temporalDifferenceLambda)
      case YellowWin =>
        // Learn from the red (defeat -> 0.0) and yellow (win -> 1.0)
        trainingData(referenceOutput = 0.0f, boardsFromEndToStart, temporalDifferenceLambda) ++
          trainingData(referenceOutput = 1.0f, boardsWithOppositeColors, temporalDifferenceLambda)
    }
  }

  private def trainingData(referenceOutput: Float,
                           boards: Seq[Board],
                           temporalDifferenceLambda: Float): Seq[TrainingData] = {
    val delta = referenceOutput - 0.5f

    // The 1st board must be associated with output = 0.5 + delta
    // The 2nd board must be associated with output = 0.5 + delta * temporalDifferenceLambda
    // The 3rd board must be associated with output = 0.5 + delta * temporalDifferenceLambda * temporalDifferenceLambda
    // etc.
    //
    // => general formula: output = 0.5 + delta * pow(temporalDifferenceLambda, index), where index is the zero-based
    //    index of the board

    for {
      (board, index) <- boards.zipWithIndex
      inputs = inputsFromBoard(board)
      output = 0.5f + delta * math.pow(temporalDifferenceLambda, index.toFloat).toFloat
    } yield TrainingData(inputs = inputs, output = output)
  }

  private def neuralNetworkFromWeights(weights: Seq[Matrix[Float]]): NeuralNetworkImpl = {
    import collection.JavaConversions._

    val multilayerPerceptron = NNFactory.mlpSigmoid(layerSizes(weights).toArray, /*addBias*/ true)

    // @todo Matrixes -> NeuralNetworkImpl

    for (layer <- multilayerPerceptron.getLayers) {
      println(s"layer connection count = ${layer.getConnections.size}")
    }

    multilayerPerceptron
  }

  private def layerSizes(weights: Seq[Matrix[_]]): Seq[Int] = {
    // @tailrec
    def layerSizes(weights: List[Matrix[_]], sizes: Seq[Int]): Seq[Int] = weights match {
      case Nil =>
        // All matrices parsed => return the accumulated sizes
        sizes
      case weightsHead :: weightsTail =>
        // Two consecutive matrices are connected to a common layer => check that they're consistent
        assert(sizes.nonEmpty && sizes.last == weightsHead.cols)
        // Use the number of rows in the matrix to determine the size of the current layer (only exception is the input
        // layer, which is taken care of by the caller)
        layerSizes(weightsTail, sizes :+ weightsHead.rows)
    }

    // Each row in a matrix corresponds to a neuron in the current layer and each column corresponds to a neuron in the
    // previous layer

    weights.toList match {
      case Nil =>
        // Empty neural network
        Seq()
      case weightsHead :: weightsTail =>
        // One matrix or more => two layers or more
        layerSizes(weightsHead :: weightsTail, sizes = Seq(weightsHead.cols))
    }
  }
}

// Inputs and output used for training the neural network
case class TrainingData(inputs: Seq[Float], output: Float) {
  assert(inputs.size == NeuralPlayer.InputCount)
}
