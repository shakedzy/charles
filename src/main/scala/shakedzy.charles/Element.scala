package shakedzy.charles

import scala.math.{Ordered, signum}

class Element[T](genes: Seq[T]) extends Ordered[Element[T]]{
  protected var _genes: Seq[T] = _
  protected var strength: Double = _
  protected var probability: Double = _

  setGenes(genes)

  protected def setGenes(genes: Seq[T]): Unit = _genes = genes
  def setStrength(strengthFunction: Seq[T] => Double): Unit = {
    strength = safeStrength(strengthFunction)
    probability = 0.0
  }

  def getStrength: Double = strength
  def getProbability: Double = probability
  def getGenes: Seq[T] = _genes

  protected def safeStrength(strengthFunction: Seq[T] => Double): Double = {
    strengthFunction(_genes) match {
      case n if n < 0.0 => throw new RuntimeException(s"Encountered negative strength for element $_genes")
      case n => n
    }
  }
  def strengthToProbability(totalStrength: Double): Unit = {
    if (totalStrength.isPosInfinity) probability = if (strength.isPosInfinity) 1.0 else 0.0
    else {
      val p = strength/totalStrength
      if (p < 0 || p > 1) throw new RuntimeException(s"Encountered problematic probability: $p for strength $strength and combined strength $totalStrength")
      else probability = p
    }
  }
  def mutate(mutationOdds: Double, values: Seq[T])(implicit random: Random): Unit = {
    val binaryStringRequiredLength = BinaryUtils.getSingleGeneBitsNum(values)
    val newGenes = _genes.map(g => {
      val bits: String = values.indexOf(g).toBinaryString
      val paddedBits: String = BinaryUtils.padBinaryString(bits,binaryStringRequiredLength)
      val mutatedBits: String = paddedBits.map(b => {
        val r = random.nextDouble()
        if (r <= mutationOdds) BinaryUtils.flipBitChar(b) else b
      })
      val newIndex = Integer.parseInt(mutatedBits,2) % values.length
      values(newIndex)
    })
    setGenes(newGenes)
  }
  override def compare(that: Element[T]): Int = signum(strength - that.getStrength).toInt
  final def canEqual(obj: Any): Boolean = obj.isInstanceOf[Element[T]]
  override def equals(obj: Any): Boolean =
    if (canEqual(obj)) _genes.hashCode() == obj.asInstanceOf[Element[T]].getGenes.hashCode()
    else false
}