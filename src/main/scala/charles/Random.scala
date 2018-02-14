package charles

class Random extends scala.util.Random {
  /** Generated a random Double, based on [[scala.util.Random]], only the number wiil
    * be in the range (0,1] rather than [0,1).
    *
    * @return a random Double
    */
  def nextPositiveDouble(): Double = {
    val nextDouble = this.nextDouble()
    if (nextDouble == 0) 1 else nextDouble
  }
}
