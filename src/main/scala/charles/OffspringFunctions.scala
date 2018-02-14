package charles

import BinaryUtils._

object OffspringFunctions {

  /** This function creates new subjects by taking the two subjects provided, transform then to a binary encoding
    * and then randomly selecting a position where they will be sliced. The new subjects are made of the first part
    * of one subject and the second part of the second subject. As an example, assume the binary encoding of the two
    * subjects are s1 = "000000" and s2 = "111111", and assume the random poition was chosen right in the middle.
    * The two new subjects will be s1_new = "000111" and s2_new = "111000"
    *
    * @param values a sequence of all values of type T which a subject in te population can have
    * @param random an instance of [[scala.util.Random]]
    * @param element1 one subject of the population, a sequence of values of type T
    * @param element2 a second subject of the population, a sequence of values of type T
    * @tparam T the type of the values (genes) of each subject
    * @return a tuple of two new subjects
    */
  def sliceAndStitch[T](values: Seq[T], random: scala.util.Random)(element1: Seq[T], element2: Seq[T]): (Seq[T],Seq[T]) = {
    val singleGeneBitsNum = getSingleGeneBitsNum(values)
    val r = random.nextInt(values.length * singleGeneBitsNum)
    val bits1 = seqToBinaryString(element1,values)
    val bits2 = seqToBinaryString(element2,values)
    val newBits1 = bits1.take(r) + bits2.drop(r)
    val newBits2 = bits2.take(r) + bits1.drop(r)
    (binaryStringToSeq(newBits1,values),binaryStringToSeq(newBits2,values))
  }

  /** This function creates two new subjects by comparing the bits of the binary encoded provided subjects (the
    * parents). If both parents have the same bit in a certain location, the offspring have a very high probability
    * of having the same bit too in that location. If the parents' bits are opposite, than the offspring's bits
    * are chosen randomly. For example, say the parents are s1 = "11000" and s2 = "11101", then with high probability
    * the offspring will be s1_new = "11100" and s2_new = "11001" (the middle and last digit are randomly chosen)
    *
    * @param values a sequence of all values of type T which a subject in te population can have
    * @param random an instance of [[scala.util.Random]]
    * @param element1 one subject of the population, a sequence of values of type T
    * @param element2 a second subject of the population, a sequence of values of type T
    * @tparam T the type of the values (genes) of each subject
    * @return a tuple of two new subjects
    */
  def parentsSimilarity[T](values: Seq[T], random: scala.util.Random)(element1: Seq[T], element2: Seq[T]): (Seq[T],Seq[T]) = {
    def createChild(bits1: String, bits2: String): String = {
      Range(0,bits1.length).map(i => {
        val r = random.nextDouble()
        val takeFrom1: Boolean = if (bits1(i) == bits2(i)) r <= 0.9 else r <= 0.5
        if (takeFrom1) bits1(i) else bits2(i)
      }).mkString("")
    }
    val bits1 = seqToBinaryString(element1,values)
    val bits2 = seqToBinaryString(element2,values)
    val newBits1 = createChild(bits1,bits2)
    val newBits2 = createChild(bits2,bits1)
    (binaryStringToSeq(newBits1,values),binaryStringToSeq(newBits2,values))
  }
}
