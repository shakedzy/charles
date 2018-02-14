import org.scalatest.FunSuite
import scala.util.Random
import com.fathzer.soft.javaluator.DoubleEvaluator
import charles._

class Examples extends FunSuite {
  /** In this example, the population is made out of 30 combinations of simple mathematical equations. Each
    * equation is constructed of four integers from 0 to 9, separated by one of four mathematical operators:
    * [+,-,/,*]. The goal of the model is to construct such mathematical equation which will yield 42.
    */
  test("Reach 42") {
    /** This is the strength function which will be supplied to the model. The strength is calculated as
      * abs(1/(42-x)) for any given x.
      * calculator is an instance of [[com.fathzer.soft.javaluator.DoubleEvaluator]] which is used to
      * compute the mathematical result out of a string of numbers and operators.
      */
    def strengthFunction(calculator: DoubleEvaluator)(values: Seq[String]): Double = {
      try {
        val result = calculator.evaluate(values.mkString(""))
        if (result == 42.0) Double.PositiveInfinity else math.abs(1.0 / (result - 42.0))
      } catch {
        case _: Exception => 0.0
      }
    }
    // Setting up some variables
    val seed = 1518336961899L
    val calculator = new DoubleEvaluator()
    val allValues = Array("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "+", "-", "*", "/")
    val random = new Random
    random.setSeed(seed)

    // Creating a random population of mathematical equations
    val population = Range(0, 30).map(_ => {
      Range(0, 7).map(i => {
        val r = if (i % 2 == 0) random.nextInt(10) else 10 + random.nextInt(4)
        allValues(r)
      })
    })

    // Setting up the model
    val model = new Model(population, allValues, strengthFunction(calculator), OffspringFunctions.sliceAndStitch(allValues, random),
      generations = 15, seed = seed)
    println("Reach 42 - Starting population:\n----------------------------------")
    population.foreach(el => println(s"${el.mkString(" ")}  =  ${calculator.evaluate(el.mkString(""))}"))

    // Population evolves
    model.evolve()

    // Best solution is chosen
    val best = model.getBest.mkString(" ")
    val result = calculator.evaluate(best)
    println(s"----------------------------------")
    println(s"Evolution end cause: ${model.getEndReason._2} [Reason ID: ${model.getEndReason._1}]")
    println(s"Iterations required: ${model.getCurrentGeneration}")
    println(s"Model's best result: $best  =  $result")
    assert(result==42)
  }
}