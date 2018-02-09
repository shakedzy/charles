package shakedzy.charles

object BinaryUtils {
  def padBinaryString(binaryString: String, requiredLength: Int): String = {
    val paddingSize = requiredLength - binaryString.length
    List.fill(paddingSize)('0').mkString("") + binaryString
  }
  def flipBitChar(bit: Char): Char = {
    if (bit == '1') '0'
    else if (bit == '0') '1'
    else throw new RuntimeException(s"No such bit-char: $bit")
  }
  def getSingleGeneBitsNum[T](values: Seq[T]): Int = (values.length - 1).toBinaryString.length
  def seqToBinaryString[T](sq: Seq[T], values: Seq[T]): String = {
    sq.map(g => values.indexOf(g).toBinaryString)
      .map(bits => BinaryUtils.padBinaryString(bits,getSingleGeneBitsNum(values))).mkString("")
  }
  def binaryStringToSeq[T](bits: String, values: Seq[T]): Seq[T] = {
    bits.grouped(getSingleGeneBitsNum(values)).toSeq.map(bits => Integer.parseInt(bits,2) % values.length).map(values(_))
  }
}
