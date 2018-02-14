package charles

import scala.math.{Ordered, signum}

/** An Element is a single subject extension in the overall population. I consists of several genes (of type T).
  * An Element has a strength which is derived from a certain predefined Strength Function (sometimes
  * refer to as Fitness Function). The higher an element's strength is, the higher are hus chance of
  * reproducing and survival. An Element can also mutate, which is the act of a spontaneous change of
  * one of its genes.
  *
  * @constructor create a new Element with genes of type T
  * @param genes a sequence of the Element's genes
  * @tparam T the type of the genes
  */
class Element[T](genes: Seq[T]) extends Ordered[Element[T]]{
  protected var _genes: Seq[T] = _
  protected var strength: Double = _
  protected var probability: Double = _

  setGenes(genes)

  def getStrength: Double = strength
  def getProbability: Double = probability
  def getGenes: Seq[T] = _genes
  protected def setGenes(genes: Seq[T]): Unit = _genes = genes

  /** Set the Element's strength based on the Strength Function. Strength must be non-negative, and can reach up
    * to +Inf. Setting the strength automatically resets the Element's survival-probability to 0.
    *
    * @param strengthFunction a function that maps a sequence of values of type T (the genes) to a non-negative
    *                         number (the Element's strength)
    */
  def setStrength(strengthFunction: Seq[T] => Double): Unit = {
    strength = strengthFunction(_genes) match {
      case n if n < 0.0 => throw new RuntimeException(s"Encountered negative strength for element ${_genes}")
      case n => n
    }
    probability = 0.0
  }

  /** Compute the Element's survival-probability, which is basically its normalized strength over the entire
    * population's combined strength
    *
    * @param totalStrength the entire's population's combined strength
    */
  def strengthToProbability(totalStrength: Double): Unit = {
    if (totalStrength.isPosInfinity) probability = if (strength.isPosInfinity) 1.0 else 0.0
    else {
      val p = strength/totalStrength
      if (p < 0 || p > 1) throw new RuntimeException(s"Encountered problematic probability: $p for strength $strength and combined strength $totalStrength")
      else probability = p
    }
  }

  /** Mutate the Element. Behind the scenes, all genes are converted to binary representations, and for each binary
    * bit, there's a probability of it flipping and changing its value. After this process, the binary representation
    * is transformed back to genes of type T.
    *
    * @param mutationOdds a number in the continuous range [0,1], representing the probability of a bit flipping
    *                     its value
    * @param values a sequence of all possible values a certain gene can have
    * @param random an instance of [[scala.util.Random]]
    */
  def mutate(mutationOdds: Double, values: Seq[T])(implicit random: scala.util.Random): Unit = {
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

  /** An override of the [[scala.math.Ordered]] extended class. This allows ordering a sequence of Elements
    * by their strength.
    *
    * @param that another Element, to which this Element is compared to
    * @return -1 if that Element has higher strength, 1 if vice-versa and 0 if both strength are equal
    */
  override def compare(that: Element[T]): Int = signum(strength - that.getStrength).toInt

  /** Verification that another object is of the same type as this Element, as only in this case can the
    * two be compared.
    *
    * @param obj Any object
    * @return True if this Element and the other object can be compared, False otherwise
    */
  final def canEqual(obj: Any): Boolean = obj.isInstanceOf[Element[T]]

  /** An override of [[java.lang.Object]], which defined if two Elements are equal or not. Only the hash of
    * their genes is being used for this - strength and probability are excluded.
    *
    * @param obj another Element
    * @return True if both Elements are equal, False otherwise
    */
  override def equals(obj: Any): Boolean =
    if (canEqual(obj)) _genes.hashCode() == obj.asInstanceOf[Element[T]].getGenes.hashCode()
    else false
}