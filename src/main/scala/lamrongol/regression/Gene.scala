package lamrongol.regression

import lamrongol.regression.Gene._

import scala.util.Random

/**
  * Created by admin on 2015/11/15.
  */


sealed abstract class Gene(val code: Int) extends Serializable {
  def calc(x: Double): Double
}

object Gene {
  val COUNT = 11
  val ALLOW_MINUS_MAX_INDEX = 7

  //Followings allow both plus and minus
  case object Unused extends Gene(0) {
    def calc(x: Double) = Double.NaN
  }

  case object Identity extends Gene(1) {
    def calc(x: Double) = x
  }

  case object Squared extends Gene(2) {
    def calc(x: Double) = x * x
  }

  case object Cubed extends Gene(3) {
    def calc(x: Double) = x * x * x
  }

  case class Exp(a: Double) extends Gene(4) {
    def calc(x: Double) = Math.exp(a * x)

    override def toString = "Exp\t" + a
  }

  case class ExpMinus(a: Double) extends Gene(5) {
    def calc(x: Double) = Math.exp(-a * x)

    override def toString = "ExpMinus\t" + a
  }

  case class ExpMinusSquared(a: Double) extends Gene(6) {
    def calc(x: Double) = Math.exp(-a * x * x)

    override def toString = "ExpMinusSquared\t" + a
  }

  //Followings don't allow zero
  case object Inverse extends Gene(7) {
    def calc(x: Double) = 1.0 / x
  }

  //Followings allow only plus
  case object Sqrt extends Gene(8) {
    def calc(x: Double) = Math.sqrt(x)
  }

  case object Log extends Gene(9) {
    def calc(x: Double) = Math.log(x)
  }

  case class Log1plus(a: Double) extends Gene(10) {
    def calc(x: Double) = Math.log(1 + a * x)

    override def toString = "Log1plus\t" + a
  }

}

object GeneManager {
  def getRandomGeneUnit(scaleFactor: Double, caseCount: Int = Gene.COUNT): Gene = {
    Random.nextInt(caseCount) match {
      case 0 => Unused
      case 1 => Identity
      case 2 => Squared
      case 3 => Cubed
      case 4 => Exp(scaleFactor / 10 + Random.nextDouble() * 9.9 * scaleFactor)
      case 5 => ExpMinus(scaleFactor / 10 + Random.nextDouble() * 9.9 * scaleFactor)
      case 6 => ExpMinusSquared(scaleFactor * scaleFactor / 10 + Random.nextDouble() * 9.9 * scaleFactor * scaleFactor)
      case 7 => Inverse
      case 8 => Sqrt
      case 9 => Log
      case 10 => Log1plus(scaleFactor / 10 + Random.nextDouble() * 9.9 * scaleFactor)
    }
  }

  def getRandomGeneUnitAllowMinus(scaleFactor: Double): Gene = {
    getRandomGeneUnit(scaleFactor, Gene.ALLOW_MINUS_MAX_INDEX)
  }

  def allowMinus(gene: Gene): Boolean = {
    gene match {
      case Inverse | Sqrt | Log | Log1plus(_) => false
      case _ => true
    }
  }

  def crossParameter(gene1: Gene, gene2: Gene): (Gene, Gene) = {
    gene1 match {
      case Exp(a) => {
        val b = gene2.asInstanceOf[Exp].a
        val sum = a + b
        //val diff = Math.max(Math.min(a, b) / 10, if (a > b) a - b else b - a)
        val average = (a + b) / 2
        (Exp(sum), Exp(average))
      }
      case ExpMinus(a) => {
        val b = gene2.asInstanceOf[ExpMinus].a
        val sum = a + b
        //val diff = Math.max(Math.min(a, b) / 10, if (a > b) a - b else b - a)
        val average = (a + b) / 2
        (ExpMinus(sum), ExpMinus(average))
      }
      case ExpMinusSquared(a) => {
        val b = gene2.asInstanceOf[ExpMinusSquared].a
        val sum = a + b
        //val diff = Math.max(Math.min(a, b) / 10, if (a > b) a - b else b - a)
        val average = (a + b) / 2
        (ExpMinusSquared(sum), ExpMinusSquared(average))
      }
      case Log1plus(a) => {
        val b = gene2.asInstanceOf[Log1plus].a
        val sum = a + b
        //val diff = Math.max(Math.min(a, b) / 10, if (a > b) a - b else b - a)
        val average = (a + b) / 2
        (Log1plus(sum), Log1plus(average))
      }
      case _ => (gene1, gene2)
    }

  }

  def withName(geneName: String, scaleFactor: String = null): Gene = {
    geneName match {
      case "Unused" => Unused
      case "Identity" => Identity
      case "Squared" => Squared
      case "Cubed" => Cubed
      case "Exp" => Exp(scaleFactor.toDouble)
      case "ExpMinus" => ExpMinus(scaleFactor.toDouble)
      case "ExpMinusSquared" => ExpMinusSquared(scaleFactor.toDouble)
      case "Inverse" => Inverse
      case "Sqrt" => Sqrt
      case "Log" => Log
      case "Log1plus" => Log1plus(scaleFactor.toDouble)
    }
  }

}