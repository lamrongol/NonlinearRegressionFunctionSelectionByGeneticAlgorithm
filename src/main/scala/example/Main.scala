package example

import lamrongol.regression.{NonlinearRegressionFunctionSelectionByGeneticAlgorithm, RegressionCalculator}

/**
  * Created by admin on 2015/12/05.
  */
object Main extends App {
  val outputFile = "result.tsv"
  //tsv file: first column is dependent variable and others are independent variables, see also SampleDataGenerator
  new NonlinearRegressionFunctionSelectionByGeneticAlgorithm("sampleData.tsv",
    //if you know each parameter is always plus (or not), this program explores functions which allows only plus value (such as sqrt, log), too
    isPlus = Array(true, true, true, false, true, true),
    checkMode = true,
    recordFile = outputFile).execute()

  //You can omit arguments as possible as following
  //new RegressionParameterSelectionByGeneticAlgorithm("sampleData.tsv").execute()


  //calculate for new data
  val calculator = new RegressionCalculator(outputFile)
  println(calculator.calculate(List(500, 600, 0.12, -18, 30000, 0.00075)))
  //You must set any value if unused parameters exist
  //println(calculator.calculate(List(500, Double.NaN, 0.12, -18, 30000, 0.00075)))

}
