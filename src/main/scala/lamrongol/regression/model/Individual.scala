package lamrongol.regression.model

import lamrongol.regression.model.Gene.Unused
import org.apache.commons.lang3.SerializationUtils

import scala.util.Random

/**
  * Created by admin on 2015/11/15.
  */
class Individual(val unitNum: Int, isPlus: Array[Boolean] = null) extends Serializable {
  var evaluation: Double = 0.0
  var R = 0.0
  var genes = new Array[Gene](unitNum)
  var coe: Array[Double] = null

  val random = new Random()

  def combine(father: Individual): (Individual, Individual) = {
    val startIdx = random.nextInt(unitNum)
    var endIdx = random.nextInt(unitNum)
    if (endIdx == startIdx) endIdx = (startIdx + 1) % unitNum

    val child1 = SerializationUtils.clone(this)
    val child2 = SerializationUtils.clone(father)

    var i = startIdx
    while (i != endIdx) {
      if (child1.genes(i).code == child2.genes(i).code) {
        val (newGene1, newGene2) = GeneManager.crossParameter(child1.genes(i), child2.genes(i))
        child1.genes(i) = newGene1
        child2.genes(i) = newGene2
      } else {
        child1.genes(i) = father.genes(i) //if (isPlus == null || isPlus(i) || GeneManager.allowMinus(father.genes(i)))
        child2.genes(i) = this.genes(i) //if (isPlus == null || isPlus(i) || GeneManager.allowMinus(this.genes(i)))
      }

      i = (i + 1) % unitNum
    }
    (child1, child2)
  }

  override def toString = {
    if (coe == null) {
      genes.mkString("\t")
    } else {
      val sb = new StringBuffer()
      val crlf = System.getProperty("line.separator");
      sb.append("#Coefficient\tFunction\tScaling Factor(if exists)").append(crlf)
      sb.append("[Intercept]\t" + coe(0)).append(crlf)
      var idx = 1
      for (i <- 0 until unitNum) {
        if (genes(i) == Unused) sb.append("Unused").append(crlf)
        else {
          sb.append(coe(idx) + "\t" + genes(i)).append(crlf)
          idx += 1
        }
      }
      sb.append("#AIC=" + evaluation).append(crlf)
      sb.append("#|R|=" + R)

      sb.toString
    }
  }
}

