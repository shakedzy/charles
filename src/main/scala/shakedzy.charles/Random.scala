package shakedzy.charles

class Random extends scala.util.Random {
  def nextPositiveDouble(): Double = {
    val nextDouble = this.nextDouble()
    if (nextDouble == 0) 1 else nextDouble
  }
}
