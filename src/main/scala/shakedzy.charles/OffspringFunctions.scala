package shakedzy.charles

import BinaryUtils._

object OffspringFunctions {
  def sliceAndStitch[T](values: Seq[T], random: scala.util.Random)(element1: Seq[T], element2: Seq[T]): (Seq[T],Seq[T]) = {
    val singleGeneBitsNum = getSingleGeneBitsNum(values)
    val r = random.nextInt(values.length * singleGeneBitsNum)
    val bits1 = seqToBinaryString(element1,values)
    val bits2 = seqToBinaryString(element2,values)
    val newBits1 = bits1.take(r) + bits2.drop(r)
    val newBits2 = bits2.take(r) + bits1.drop(r)
    (binaryStringToSeq(newBits1,values),binaryStringToSeq(newBits2,values))
  }
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
