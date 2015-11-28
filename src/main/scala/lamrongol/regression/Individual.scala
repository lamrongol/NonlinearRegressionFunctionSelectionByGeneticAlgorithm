package lamrongol.regression

import org.apache.commons.lang3.SerializationUtils

import scala.util.Random

/**
  * Created by admin on 2015/11/15.
  */
class Individual(val unitNum: Int, isPlus: Array[Boolean] = null) extends Serializable {
  var evaluation: Double = 0.0
  var R = 0.0
  var genes = new Array[GeneUnit.Value](unitNum)
  var coe: Array[Double] = null

  val random = new Random()

  def randomInitialize() = {
    for (i <- 0 until unitNum) {
      genes(i) = if (isPlus != null && isPlus(i)) GeneManager.getRandomGeneUnit() else GeneManager.getRandomGeneUnitAllowMinus()
    }
  }

  def combine(father: Individual): (Individual, Individual) = {
    val startIdx = random.nextInt(unitNum)
    var endIdx = random.nextInt(unitNum)
    if (endIdx == startIdx) endIdx = (startIdx + 1) % unitNum

    val child1 = SerializationUtils.clone(this)
    val child2 = SerializationUtils.clone(father)

    var i = startIdx
    while (i != endIdx) {
      if (isPlus != null && isPlus(i) == false && !allowMinus(father.genes(i))) {} else child1.genes(i) = father.genes(i)
      if (isPlus != null && isPlus(i) == false && !allowMinus(this.genes(i))) {} else child2.genes(i) = this.genes(i)

      i = (i + 1) % unitNum
    }
    (child1, child2)
  }

  def mutate() = {
    val mutationIdx = random.nextInt(unitNum)
    genes(mutationIdx) = if (isPlus != null && isPlus(mutationIdx)) GeneManager.getRandomGeneUnit() else GeneManager.getRandomGeneUnitAllowMinus()
  }

  def allowMinus(geneUnit: GeneUnit.Value): Boolean = geneUnit.id < GeneUnit.ALLOW_MINUS_MAX_INDEX

}

