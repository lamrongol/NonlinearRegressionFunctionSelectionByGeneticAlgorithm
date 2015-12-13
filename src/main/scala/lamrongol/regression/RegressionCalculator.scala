package lamrongol.regression

import java.io.File

import lamrongol.regression.model.{GeneManager, Gene}
import Gene.Unused
import org.apache.commons.io.FileUtils

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by admin on 2015/11/16.
  */
class RegressionCalculator(tsvFile: String) {

  private val lines = {
    val resource = getClass.getResource(tsvFile)
    if (resource != null) {
      val source = Source.fromURL(resource)
      val it = source.getLines()
      val lines = new ArrayBuffer[String]()
      while (it.hasNext) {
        lines += it.next()
      }
      lines.toArray
    } else {
      FileUtils.readLines(new File(tsvFile)).toArray(new Array[String](0))
    }
  }

  private val coes = new ArrayBuffer[Coefficient]
  private var r = 0.0

  var idx = 0
  for (line <- lines) {
    if (line.startsWith("#")) {
      val tmp = line.split("=")
      if (tmp(0) == "#|R|") r = tmp(1).toDouble
    } else {
      val tmp = line.split("\t")
      if (tmp(0) == "[Intercept]") {
        coes += new Coefficient(tmp(1).toDouble, null)
      }
      else {
        val gene = if (tmp(0) == "Unused") Unused else if (tmp.length == 2) GeneManager.withName(tmp(1)) else GeneManager.withName(tmp(1), tmp(2))
        val value = if (gene == Unused) Double.NaN else tmp(0).toDouble
        coes += new Coefficient(value, gene)
      }
    }
  }

  def calculate(parameters: List[Double]): Double = {
    var sum = coes(0).value
    for (i <- 1 until coes.length) {
      if (coes(i).gene != Unused) {
        sum += coes(i).value * coes(i).gene.calc(parameters(i - 1))
      }
    }
    sum
  }

  def getR: Double = r
}

class Coefficient(val value: Double, val gene: Gene)