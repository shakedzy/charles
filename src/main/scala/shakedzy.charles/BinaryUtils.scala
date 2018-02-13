package shakedzy.charles

object BinaryUtils {
  /** Pads a binary String with additional zeros.
    *
    * {{{
    * scala> padBinaryString("101",5)
    * res2: String = 00101
    * }}}
    *
    * @param binaryString a binary representation as a string
    * @param requiredLength the number of digits required in the output binary string
    * @return a binary representation as a string with additional zeros
    */
  def padBinaryString(binaryString: String, requiredLength: Int): String = {
    val paddingSize = requiredLength - binaryString.length
    List.fill(paddingSize)('0').mkString("") + binaryString
  }

  /** Flips the character '1' to '0' and vice-versa. Throws an error for any other value.
    *
    * @param bit a character, either '1' or '0'
    * @return the flipped binary character
    */
  def flipBitChar(bit: Char): Char = {
    if (bit == '1') '0'
    else if (bit == '0') '1'
    else throw new RuntimeException(s"No such bit-char: $bit")
  }

  /** Returns the number of digits required to represent all values in the provided sequence as
    * binary encoding representations. For example, if the values are ("a","b","c","d"), they will be
    * transformed to (0,1,10,11) in binary, and so 2 digits are required. For ("Z","Y","X","W","V"),
    * the representation will be (0,1,10,11,100), and so 3 digits are required.
    *
    * @param values a sequence of all values to be represented in binary format
    * @tparam T the type of the values in the sequence
    * @return the number of digits required to represent the values in binary encoding
    */
  def getSingleGeneBitsNum[T](values: Seq[T]): Int = (values.length - 1).toBinaryString.length

  /** Convert each value of type T in a sequence to a binary encoding of it, based on its index in the
    * values sequence.
    *
    * {{{
    * scala> seqToBinaryString(Seq("X","Z"), Seq("X","Y","Z"))
    * res3: String = 0010
    *
    * scala> seqToBinaryString(Seq("Y","Z"), Seq("X","Y","Z"))
    * res4: String = 0110
    *
    * scala> seqToBinaryString(Seq("Z","Z"), Seq("X","Y","Z"))
    * res5: String = 1010
    * }}}
    *
    * @param sq the sequence of values to encode to a binary representation
    * @param values all possible values each element of the sequence can have
    * @tparam T the type of each element of the sequences
    * @return a string made of binary encoding of the sequence provided
    */
  def seqToBinaryString[T](sq: Seq[T], values: Seq[T]): String = {
    sq.map(g => values.indexOf(g).toBinaryString)
      .map(bits => padBinaryString(bits,getSingleGeneBitsNum(values))).mkString("")
  }

  /** Convert a string of a binary representation to a sequence of elements of type T. This is the opposite
    * of getSingleGeneBitsNum.
    *
    * {{{
    * scala> binaryStringToSeq("0010",Seq("X","Y","Z"))
    * res7: Seq[String] = List(X, Z)
    *
    * scala> binaryStringToSeq("0110",Seq("X","Y","Z"))
    * res8: Seq[String] = List(Y, Z)
    *
    * scala> binaryStringToSeq("1010",Seq("X","Y","Z"))
    * res9: Seq[String] = List(Z, Z)
    * }}}
    *
    * @param bits a string of a binary encoding
    * @param values all values of type T which a subject in te population can have
    * @tparam T the type of each element of the sequence
    * @return a sequence of elements of type T, decoded from the provided string
    */
  def binaryStringToSeq[T](bits: String, values: Seq[T]): Seq[T] = {
    bits.grouped(getSingleGeneBitsNum(values)).toSeq.map(bits => Integer.parseInt(bits,2) % values.length).map(values(_)).toList
  }
}
