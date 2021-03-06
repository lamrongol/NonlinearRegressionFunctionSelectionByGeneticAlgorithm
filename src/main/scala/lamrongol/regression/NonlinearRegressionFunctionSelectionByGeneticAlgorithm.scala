package lamrongol.regression

import java.io.PrintWriter

import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}
import lamrongol.regression.model.{Individual, GeneManager, Evaluation, Gene}
import Gene.Unused
import org.apache.commons.io.FilenameUtils
import org.apache.commons.math3.linear.SingularMatrixException
import org.apache.commons.math3.stat.StatUtils
import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by admin on 2015/11/15.
  */

class NonlinearRegressionFunctionSelectionByGeneticAlgorithm(tsvFile: String, criterionColumn: Int = 0, parameterColumnStart: Int = 1,
                                                             parameterColumnEnd: Int = -1,
                                                             evaluationWay: Evaluation.Value = Evaluation.Aic, isPlus: Array[Boolean] = null,
                                                             var recordFile: String = null,

                                                             val INDIVIDUAL_NUM: Int = 500,
                                                             val TOP_SELECTION_NUM: Int = 30,
                                                             val MUTATION_RATE: Double = 0.03,

                                                             val MAX_LOOP_COUNT: Int = 1000,
                                                             val MIN_LOOP_COUNT: Int = 10,
                                                             val STOP_DIFF_RATE: Double = 0.000001,
                                                             val MAX_DATA_NUM: Int = -1,
                                                             val checkMode: Boolean = false
                                                            ) {
  println(tsvFile)
  if (recordFile == null) recordFile = FilenameUtils.getBaseName(tsvFile) + "_result.tsv"
  val random = new Random()
	var dependentList = scala.collection.mutable.ArrayBuffer.empty[Double]
	var parametersList = scala.collection.mutable.ArrayBuffer.empty[mutable.Buffer[Double]]
  val absValuesList = scala.collection.mutable.ArrayBuffer.empty[mutable.Buffer[Double]]

  //val file = "D:\\FxData\\USD_JPY\\features.tsv"

  implicit object MyFormat extends DefaultCSVFormat {
    override val quoteChar = ' '
    override val delimiter = '\t'
  }

  var criterionAverage = 0.0

  val reader = CSVReader.open(tsvFile)
  val it = reader.iterator
  var count = 0
  while (it.hasNext) {
    val nextLine = it.next
    //if (nextLine(1) == "BUY") {
    var dependent = nextLine(0).toDouble
    dependentList += dependent
    val endIdx = if (parameterColumnEnd == -1) nextLine.size else parameterColumnEnd
    val allParameters = nextLine.slice(parameterColumnStart, endIdx).map(_.toDouble).toBuffer
    //allParameters(0) = Math.exp(allParameters(0))
    parametersList += allParameters
    if (count == 0) {
      for (i <- 0 until allParameters.length) {
        absValuesList += scala.collection.mutable.ArrayBuffer.empty[Double]
      }
    }

    for (i <- 0 until allParameters.length) {
      absValuesList(i) += Math.abs(allParameters(i))
    }

    count += 1
  }
	if (MAX_DATA_NUM > 0) {
		val startIdx = parametersList.size - MAX_DATA_NUM
		if (startIdx > 0) {
			val endIdx = parametersList.size
			parametersList = parametersList.slice(startIdx, endIdx)
			dependentList = dependentList.slice(startIdx, endIdx)
		}
	}

  val dependents = dependentList.toArray
  var parameterCount = parametersList(0).size
  if (parameterCount != isPlus.length) throw new Exception("isPlus length is differ from parameter count")

  val medians = new Array[Double](parameterCount)
  for (i <- 0 until parameterCount) {
    medians(i) = StatUtils.percentile(absValuesList(i).toArray, 50)
  }
  //val yMedians = StatUtils.percentile(dependents.map(Math.abs(_)), 50)

  val scaleFactors = medians.map(1.0 / _)

  def execute(): Double = {
    var individuals = new Array[Individual](INDIVIDUAL_NUM)
    for (i <- 0 until INDIVIDUAL_NUM) {
      individuals(i) = new Individual(parameterCount, isPlus)
      for (j <- 0 until parameterCount) {
        individuals(i) genes (j) = if (isPlus != null && isPlus(j)) GeneManager.getRandomGeneUnit(scaleFactors(j))
        else GeneManager.getRandomGeneUnitAllowMinus(scaleFactors(j))
      }
    }

    var loopCount = 0
    var evaluationDiffRate = Double.MaxValue

    for (i <- 0 until INDIVIDUAL_NUM) {
      calcEvaluation(individuals(i))
    }
    individuals = individuals.sortBy(_.evaluation)
    var preBestEvaluation = individuals(0).evaluation

    while (loopCount < MIN_LOOP_COUNT || (evaluationDiffRate > STOP_DIFF_RATE && loopCount < MAX_LOOP_COUNT)) {
      println(s"loopCount:$loopCount")
      var nextIndividuals = new ArrayBuffer[Individual]()
      for (i <- 0 until TOP_SELECTION_NUM) {
        nextIndividuals += individuals(i)
      }
      if (loopCount % 10 == 0) {
        //nextIndividuals += cleanLowContributionRateParameters(individuals(0))
      }

      for (i <- TOP_SELECTION_NUM until INDIVIDUAL_NUM) {
        if (random.nextDouble() < 30.0 / i) nextIndividuals += individuals(i)
      }

      val survivorNum = nextIndividuals.size
      println(s"survivorNum=$survivorNum")
      while (nextIndividuals.size < INDIVIDUAL_NUM) {
        val motherIdx = random.nextInt(survivorNum) //randomIntByGaussian(survivorNum) //
        var fatherIdx = random.nextInt(survivorNum) //randomIntByGaussian(survivorNum) //
        if (fatherIdx == motherIdx) fatherIdx = (fatherIdx + 1) / survivorNum
        val (child1, child2) = nextIndividuals(motherIdx).combine(nextIndividuals(fatherIdx))
        nextIndividuals += child1
        if (nextIndividuals.size < INDIVIDUAL_NUM) nextIndividuals += child2
      }
      for (i <- 1 until INDIVIDUAL_NUM) {
        if (random.nextDouble() < MUTATION_RATE) {
          val mutationIdx = random.nextInt(parameterCount)
          nextIndividuals(i).genes(mutationIdx) = if (isPlus != null && isPlus(mutationIdx)) GeneManager.getRandomGeneUnit(scaleFactors(mutationIdx))
          else GeneManager.getRandomGeneUnitAllowMinus(scaleFactors(mutationIdx))
        }
      }

      for (i <- 0 until INDIVIDUAL_NUM) {
        calcEvaluation(nextIndividuals(i))
      }
      nextIndividuals = nextIndividuals.sortBy(_.evaluation)
      val bestEvaluation = nextIndividuals(0).evaluation

      val best = nextIndividuals(0)
      println()
      println(best)

      //println(preBestEvaluation, bestEvaluation, Math.abs(preBestEvaluation - bestEvaluation), Math.abs(preBestEvaluation - bestEvaluation) / preBestEvaluation)
      evaluationDiffRate = Math.abs(preBestEvaluation - bestEvaluation) / preBestEvaluation

      preBestEvaluation = bestEvaluation
      loopCount += 1
      individuals = nextIndividuals.toArray
      println("------------------------------------------------------------------------")
    }

    val best = individuals(0)

    val pw = new PrintWriter(recordFile)
    pw.println(best)
    pw.close()

    if (checkMode) {
      val calculator = new RegressionCalculator(recordFile)
      var errorRateList = scala.collection.mutable.ArrayBuffer.empty[Double]
      println("actual\tcalculated\terrorRate")
      for (i <- 0 until parametersList.size) {
        val parameters = parametersList(i)

        val actual = dependents(i)
        val calculated = calculator.calculate(parameters.toList)
        val errorRate = Math.abs(actual - calculated) / (Math.abs(actual) + Double.MinPositiveValue) * 100 //not to be infinity
        errorRateList += errorRate

        println(s"$actual\t$calculated\t$errorRate")
      }
      println("Error rate median = " + StatUtils.percentile(errorRateList.toArray, 50) + "%")
    }

    println()
    println("output:" + recordFile)

    return best.R
  }

  /**
    * nまでの整数で小さい数ほど出現しやすい乱数
    * 実験の結果うまくいかない
    */
  def randomIntByGaussian(n: Int): Int = {
    val gaussian = random.nextGaussian()
    Math.min(n - 1, n * gaussian * gaussian).toInt
  }

  def calcEvaluation(individual: Individual): Unit = {
    val parameterCount = individual.genes.count(_ != Unused)
    val dataCount = parametersList.size
    val parametersArray = Array.ofDim[Double](dataCount, parameterCount)
    for (i <- 0 until parametersList.size) {
      val tmpList = new ArrayBuffer[Double]
      for (j <- 0 until individual.genes.length) {
        if (individual.genes(j) != Unused) {
          tmpList += individual.genes(j).calc(parametersList(i)(j))
        }
      }
      parametersArray(i) = tmpList.toArray
    }

    val reg = new OLSMultipleLinearRegression()
    reg.newSampleData(dependents, parametersArray)
    //    try {
    val coe = reg.estimateRegressionParameters()
    individual.coe = coe
    /*
        } catch {
          case e: SingularMatrixException => {
            if (individual.genes.count(_ == Unused) == 0) {
              println("Your data is singular")
            } else {
              individual.evaluation = Double.MaxValue
              return
            }
    }
    */

    val degreeOfFreedomSum = individual.genes.map(_.degreeOfFreedom).sum
    val aic = dataCount * Math.log(reg.calculateResidualSumOfSquares() / dataCount) + 2 * (degreeOfFreedomSum + 1)

    individual.R = Math.sqrt(reg.calculateAdjustedRSquared())
    /*
        val r = reg.calculateAdjustedRSquared()
        println("決定係数=" + r)
        println("AIC=" + aic)
        println()
    */

    individual.evaluation = evaluationWay match {
      case Evaluation.Aic => aic
    }
  }

}

