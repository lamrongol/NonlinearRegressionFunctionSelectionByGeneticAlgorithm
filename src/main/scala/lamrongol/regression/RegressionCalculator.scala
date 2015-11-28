package lamrongol.regression

import java.io.File

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
			if (tmp(0) == "#R") r = tmp(1).toDouble
		} else {
			val tmp = line.split("\t")
			val func = if (tmp(0) == "[Intercept]") null else GeneUnit.withName(tmp(0))
			val value = if (func == GeneUnit.None) Double.NaN else tmp(1).toDouble
			coes += new Coefficient(value, func)
		}
	}

	def calculate(parameters: List[Double]): Double = {
		var sum = coes(0).value
		var idx = 0
		for (i <- 1 until coes.length) {
			if (coes(i).func != GeneUnit.None) {
				sum += coes(i).value * GeneManager.apply(parameters(i - 1), coes(i).func)
			}
		}
		sum
	}

	def getR: Double = r
}

class Coefficient(val value: Double, val func: GeneUnit.Value) {

}
