package org.bruchez.connectfour.core

import com.github.neuralnetworks.architecture.Matrix
import com.github.neuralnetworks.architecture.types.NNFactory
import com.github.neuralnetworks.calculation.OutputError
import com.github.neuralnetworks.training._
import com.github.neuralnetworks.training.events.{ EarlyStoppingListener, LogTrainingListener }
import com.github.neuralnetworks.training.random.{ MersenneTwisterRandomInitializer, NNRandomInitializer }
import org.specs2.mutable._

class XorTest extends Specification {
  "BackPropagationTrainer" should {
    "learn the XOR function" in {
      // create multi layer perceptron with one hidden layer and bias
      val mlp = NNFactory.mlpSigmoid(Array[Int](2, 8, 1), true)

      // create training and testing input providers
      val trainingInput = new XorInputProvider(10000)
      val testingInput = new XorInputProvider(4)

      // create backpropagation trainer for the network
      val bpt = TrainerFactory.backPropagation(
        mlp,
        trainingInput,
        testingInput,
        new XorOutputError(),
        new NNRandomInitializer(new MersenneTwisterRandomInitializer(-0.01f, 0.01f)),
        1f,
        0.5f,
        0f,
        0f)

      // add logging
      bpt.addEventListener(new LogTrainingListener(Thread.currentThread().getStackTrace()(1).getMethodName))

      // early stopping
      bpt.addEventListener(new EarlyStoppingListener(testingInput, 1000, 0.1f))

      // train
      bpt.train()

      // test
      bpt.test()

      bpt.getOutputError.getTotalNetworkError must beCloseTo(0.0f, 0.1f)
    }
  }
}

class XorInputProvider(val inputSize: Int, val input: TrainingInputData) extends TrainingInputProvider {
  private var currentInput: Int = 0

  def this(inputSize: Int) {
    this(inputSize, new TrainingInputDataImpl(new Matrix(2, 1), new Matrix(1, 1)))
  }

  override def getNextInput: TrainingInputData = {
    if (currentInput < inputSize) {
      currentInput % 4 match {
        case 0 =>
          input.getInput.set(0, 0, 0)
          input.getInput.set(1, 0, 0)
          input.getTarget.set(0, 0, 0)
        case 1 =>
          input.getInput.set(0, 0, 0)
          input.getInput.set(1, 0, 1)
          input.getTarget.set(0, 0, 1)
        case 2 =>
          input.getInput.set(0, 0, 1)
          input.getInput.set(1, 0, 0)
          input.getTarget.set(0, 0, 1)
        case 3 =>
          input.getInput.set(0, 0, 1)
          input.getInput.set(1, 0, 1)
          input.getTarget.set(0, 0, 0)
      }

      currentInput += 1

      input
    } else {
      null
    }
  }

  override def reset(): Unit = {
    currentInput = 0
  }

  override def getInputSize: Int = inputSize
}

class XorOutputError extends OutputError {
  private var networkError: Float = 0.0f
  private var size: Int = 0

  override def addItem(networkOutput: Matrix, targetOutput: Matrix): Unit = {
    for (i <- 0 until targetOutput.getColumns) {
      networkError += Math.abs(Math.abs(networkOutput.get(0, i)) - Math.abs(targetOutput.get(0, i)))
      size += 1
    }
  }

  override def getTotalNetworkError: Float = if (size > 0) networkError / size else 0

  override def getTotalErrorSamples: Int = size

  override def getTotalInputSize: Int = size

  override def reset(): Unit = {
    networkError = 0
    size = 0
  }
}
