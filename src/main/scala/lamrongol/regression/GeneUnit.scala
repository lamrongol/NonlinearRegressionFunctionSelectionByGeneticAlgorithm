package lamrongol.regression

import scala.util.Random

/**
  * Created by admin on 2015/11/15.
  */
object GeneUnit extends Enumeration {
  val ALLOW_MINUS_MAX_INDEX = 4
  val None, Identity, Squared, Cubed, //Exp, ExpMinus, ExpMinusSquare,
  Inverse, //Not allow zero
  Sqrt, Log //Can't used for zero or minus
  = Value
}

object GeneManager {
  def apply(value: Double, func: GeneUnit.Value): Double = {
    func match {
      case GeneUnit.None => Double.NaN
      case GeneUnit.Identity => value
      case GeneUnit.Squared => value * value
      case GeneUnit.Cubed => value * value * value
      //      case GeneUnit.Exp => Math.exp(value)
      //      case GeneUnit.ExpMinus => Math.exp(-value)
      //      case GeneUnit.ExpMinusSquare => Math.exp(-(value * value))
      case GeneUnit.Inverse => 1.0 / value
      case GeneUnit.Sqrt => Math.sqrt(value)
      case GeneUnit.Log => Math.log(value)
    }
  }

  val random = new Random()

  def getRandomGeneUnit(): GeneUnit.Value = {
    GeneUnit(random.nextInt(GeneUnit.maxId))
  }

  def getRandomGeneUnitAllowMinus(): GeneUnit.Value = {
    GeneUnit(random.nextInt(GeneUnit.ALLOW_MINUS_MAX_INDEX))
  }

}