package lamrongol.regression

import java.io.PrintWriter

import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}
import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by admin on 2015/11/15.
  */

class RegressionParameterSelectionByGeneticAlgorithm(tsvFile: String, criterionColumn: Int = 0, parameterColumnStart: Int = 1,
                                                     parameterColumnEnd: Int = -1,
                                                     evaluationWay: Evaluation.Value = Evaluation.Aic, isPlus: Array[Boolean] = null,
                                                     val recordFile: String = "result.tsv",

                                                     val INDIVIDUAL_NUM: Int = 500,
                                                     val TOP_SELECTION_NUM: Int = 30,
                                                     val MUTATION_RATE: Double = 0.03,

                                                     val MAX_LOOP_COUNT: Int = 1000,
                                                     val MIN_LOOP_COUNT: Int = 10,
                                                     val STOP_DIFF_RATE: Double = 0.000001
                                                    ) {
  val random = new Random()
  val profitList = scala.collection.mutable.ArrayBuffer.empty[Double]
  val parametersList = scala.collection.mutable.ArrayBuffer.empty[mutable.Buffer[Double]]

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
    //if (profit > 0) profit = Math.log(1 + profit) else -Math.log(1 - profit)
    //profitList += (if (nextLine(0).toDouble > 0) 100 else -100)
    val nextLine = it.next
    //if (nextLine(1) == "BUY") {
    var profit = nextLine(0).toDouble
    profitList += profit
    val endIdx = if (parameterColumnEnd == -1) nextLine.size else parameterColumnEnd
    val allParameters = nextLine.slice(parameterColumnStart, endIdx).map(_.toDouble).toBuffer
    //allParameters(0) = Math.exp(allParameters(0))
    parametersList += allParameters

    count += 1
  }
  val profits = profitList.toArray
  var parameterCount = parametersList(0).size

  def execute(): Double = {
    var individuals = new Array[Individual](INDIVIDUAL_NUM)
    for (i <- 0 until INDIVIDUAL_NUM) {
      individuals(i) = new Individual(parameterCount, isPlus)
      individuals(i).randomInitialize()
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
        if (random.nextDouble() < MUTATION_RATE) nextIndividuals(i).mutate
      }

      for (i <- 0 until INDIVIDUAL_NUM) {
        calcEvaluation(nextIndividuals(i))
      }
      nextIndividuals = nextIndividuals.sortBy(_.evaluation)
      val bestEvaluation = nextIndividuals(0).evaluation

      val best = nextIndividuals(0)
      println(best.genes.mkString("\t"))
      println(best.coe.mkString("\t"))
      println("AIC=" + bestEvaluation)
      println("R=" + nextIndividuals(0).R)

      //println(preBestEvaluation, bestEvaluation, Math.abs(preBestEvaluation - bestEvaluation), Math.abs(preBestEvaluation - bestEvaluation) / preBestEvaluation)
      evaluationDiffRate = Math.abs(preBestEvaluation - bestEvaluation) / preBestEvaluation

      preBestEvaluation = bestEvaluation
      loopCount += 1
      individuals = nextIndividuals.toArray
      println("------------------------------------------------------------------------")
    }

    val best = individuals(0)
    println(best.genes.mkString("\t"))
    println(best.coe.mkString("\t"))
    println("#AIC=" + best.evaluation)
    println("#R=" + best.R)
    println(recordFile)
    val pw = new PrintWriter(recordFile)

    pw.println("#AIC=" + best.evaluation)
    pw.println("#R=" + best.R)
    pw.println("[Intercept]\t" + best.coe(0))
    var idx = 1
    for (i <- 0 until best.unitNum) {
      if (best.genes(i) == GeneUnit.None) pw.println("None" + "\t" + "None")
      else {
        pw.println(best.genes(i) + "\t" + best.coe(idx))
        idx += 1
      }
    }
    pw.close()
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
    val parameterCount = individual.genes.count(_ != GeneUnit.None)
    val dataCount = parametersList.size
    val parametersArray = Array.ofDim[Double](dataCount, parameterCount)
    for (i <- 0 until parametersList.size) {
      val tmpList = new ArrayBuffer[Double]
      for (j <- 0 until individual.genes.length) {
        if (individual.genes(j) != GeneUnit.None) {
          tmpList += GeneManager.apply(parametersList(i)(j), individual.genes(j))
        }
      }
      parametersArray(i) = tmpList.toArray
    }

    val reg = new OLSMultipleLinearRegression()
    reg.newSampleData(profits, parametersArray)
    val coe = reg.estimateRegressionParameters()
    val aic = dataCount * Math.log(reg.calculateResidualSumOfSquares() / dataCount) + 2 * (parameterCount + 1)

    individual.coe = coe
    individual.R = reg.calculateAdjustedRSquared()
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

  /*

    def cleanLowContributionRateParameters(individual: Individual): Individual = {
      val newIndividual = SerializationUtils.clone(individual)
      val parameterCount = newIndividual.genes.count(_ != GeneUnit.None)
      val dataCount = parametersList.size
      val contributionRates = new Array[Double](newIndividual.genes.length)

      for (i <- 0 until parametersList.size) {
        val contributions = new Array[Double](newIndividual.genes.length)
        var idx = 0
        var sum = newIndividual.coe(idx)
        for (j <- 0 until newIndividual.genes.length if (newIndividual.genes(j) != GeneUnit.None)) {
          idx += 1
          contributions(j) = GeneManager.apply(parametersList(i)(j), newIndividual.genes(j))
          contributions(j) *= newIndividual.coe(idx)
          sum += Math.abs(contributions(j))
        }
        for (j <- 0 until newIndividual.genes.length if (newIndividual.genes(j) != GeneUnit.None)) {
          contributionRates(j) += Math.abs(contributions(j)) / sum
        }
      }

      print("ContiributionRates: ")
      for (j <- 0 until newIndividual.genes.length if (newIndividual.genes(j) != GeneUnit.None)) {
        contributionRates(j) /= dataCount
        print(contributionRates(j) + "\t")
        if (contributionRates(j) < 0.05 / parameterCount) newIndividual.genes(j) = GeneUnit.None
      }
      println()
      newIndividual
    }
  */

}

