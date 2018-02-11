import org.scalatest.FunSuite
import scala.util.Random
import com.fathzer.soft.javaluator.DoubleEvaluator
import shakedzy.charles._

class Examples extends FunSuite {
  test("Reach 42") {
    def strengthFunction(calculator: DoubleEvaluator)(values: Seq[String]): Double = {
      try {
        val result = calculator.evaluate(values.mkString(""))
        if (result == 42.0) Double.PositiveInfinity else math.abs(1.0 / (result - 42.0))
      } catch {
        case _: Exception => 0.0
      }
    }
    val seed = 1518336961899L
    val calculator = new DoubleEvaluator()
    val allValues = Array("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "+", "-", "*", "/")
    val random = new Random
    random.setSeed(seed)
    val population = Range(0, 30).map(_ => {
      Range(0, 7).map(i => {
        val r = if (i % 2 == 0) random.nextInt(10) else 10 + random.nextInt(4)
        allValues(r)
      })
    })
    val model = new Model(population, allValues, strengthFunction(calculator), OffspringFunctions.sliceAndStitch(allValues, random),
      generations = 40, seed = seed)
    println("Reach 42 - Starting population:\n----------------------------------")
    population.foreach(el => println(s"${el.mkString(" ")}  =  ${calculator.evaluate(el.mkString(""))}"))
    model.evolve()
    val best = model.getBest.mkString(" ")
    val result = calculator.evaluate(best)
    println(s"----------------------------------\nModel's best result:\n$best  =  $result")
    assert(result==42)
  }
}