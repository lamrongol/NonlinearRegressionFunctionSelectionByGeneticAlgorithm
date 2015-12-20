package example

import java.io.PrintWriter

import scala.util.Random

/**
  * Created by admin on 2015/12/05.
  */
object SampleDataGenerator extends App {
  val DATA_NUM = 500

  val pw = new PrintWriter("sampleData.tsv")

  for (i <- 0 until DATA_NUM) {
    val x1 = 1000 + Random.nextDouble() * 9000
    val x2 = 50 + Random.nextDouble() * 6000
    val x3 = 0.10 + Random.nextDouble() / 10
    val x4 = (Random.nextDouble() - 0.5) * 124
    val x5 = Random.nextDouble() * 800000 + 2000
    val x6 = (Random.nextDouble() * 9 + 1) / 8000
    val noise = Random.nextGaussian() * 1000

    val y = x1 + Math.exp(72 * x3) + x4 * x4 + Math.sqrt(x5) + 1 / x6 + noise
    pw.println(List(y, x1, x2, x3, x4, x5, x6).mkString("\t"))
  }
  pw.close()
}
