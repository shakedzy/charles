package shakedzy.charles

import scala.util.Sorting
import scala.math._
import scala.util.control.Breaks._

class Model[T](population: Seq[Seq[T]],
               allValues: Seq[T],
               strengthFunction: Seq[T] => Double,
               offspringFunction: (Seq[T],Seq[T]) => (Seq[T],Seq[T]),
               elitismRatio: Double = 0.1,
               mutationsOdds: Double = 0.001,
               generations: Int = 10,
               seed: Long = System.currentTimeMillis()) {

  protected var _strengthFunction: Seq[T] => Double = _
  protected var _offspringFunction: (Seq[T],Seq[T]) => (Seq[T],Seq[T]) = _
  protected var _elitismRatio: Double = _
  protected var _mutationsOdds: Double = _
  protected var _generations: Int = _
  protected var _seed: Long = _
  protected var elements: Array[Element[T]] = _

  protected implicit val random: Random = new Random
  //private implicit val order: Ordering[Element[T]] = Ordering[Element[T]].reverse

  setStrengthFunction(strengthFunction)
  setOffspringFunction(offspringFunction)
  setElitismRatio(elitismRatio)
  setMutationOdds(mutationsOdds)
  setGenerations(generations)
  setSeed(seed)
  setPopulation(population)

  def setStrengthFunction(strengthFunction: Seq[T] => Double): Unit = _strengthFunction = strengthFunction
  def setOffspringFunction(offspringFunction: (Seq[T],Seq[T]) => (Seq[T],Seq[T])): Unit = _offspringFunction = offspringFunction
  def setElitismRatio(elitismRatio: Double): Unit =
    if (elitismRatio < 0 || elitismRatio > 1) throw new RuntimeException("Elitism Ratio must be in the range [0,1]")
    else _elitismRatio = elitismRatio
  def setMutationOdds(mutationsOdds: Double): Unit =
    if (mutationsOdds < 0 || mutationsOdds > 1) throw new RuntimeException("Mutations Odds must be in the range [0,1]")
    else _mutationsOdds = mutationsOdds
  def setGenerations(generations: Int): Unit =
    if (generations <= 0) throw new RuntimeException("Generations number must be greater than 0")
    else _generations = generations
  def setSeed(seed: Long): Unit = {
    _seed = seed
    random.setSeed(seed)
  }
  protected def setPopulation(population: Seq[Seq[T]]): Unit = {
    if (population.map(el => el.length).forall(len => len == population.head.length))
      elements = population.map(subject => new Element(subject)).toArray
    else throw new RuntimeException("All subjects in the population must have the same size.")
  }

  def getStrengthFunction: Seq[T] => Double = _strengthFunction
  def getElitismRatio: Double = _elitismRatio
  def getMutationOdds: Double =  _mutationsOdds
  def getGenerations: Int = _generations
  def getSeed: Long = _seed
  def getElements: Array[Element[T]] = elements
  def getPopulation: Seq[Seq[T]] = elements.map(_.getGenes)
  def getOffspringFunction: (Seq[T],Seq[T]) => (Seq[T],Seq[T]) = _offspringFunction

  protected def killMisfits(): Unit = elements = elements.filter(_.getStrength > 0.0)

  protected def selectElement(ignoreThisElement: Option[Element[T]] = None): Element[T] = {
    val r = ignoreThisElement match {
      case None => random.nextPositiveDouble()
      case Some(el) => random.nextPositiveDouble() * (1.0 - el.getProbability)
    }
    var probSum: Double = 0.0
    var selected: Element[T] = null
    breakable {
      for (element <- elements) {
        if (ignoreThisElement.isEmpty || ignoreThisElement.get != element) {
          val p = element.getProbability
          if (probSum + p >= r) {
            selected = element
            break()
          } else {
            probSum += p
          }
        }
      }
    }
    if (selected == null) selected = elements.last
    selected
  }

  def reset(): Unit = setPopulation(population)
  def getBest: Seq[T] = elements.head.getGenes
  def getBest(n: Int): Seq[Seq[T]] = elements.take(n).map(_.getGenes)

  def evolve(): Unit = {
    breakable {
      for (g <- Range(0,_generations+1)) {
        if (g > 0) {
          val elNum = elements.length
          killMisfits()
          if (elements.length < 2) break()
          val elitistsNum = round(_elitismRatio * elNum).toInt
          var nextGen = elements.take(elitistsNum)
          val remainingNum = floor((elNum - elitistsNum)/2).toInt
          for (_ <- Range(0,remainingNum)) {
            val father = selectElement()
            val mother = selectElement(ignoreThisElement = Option(father))
            val (child1genes, child2genes) = _offspringFunction(father.getGenes,mother.getGenes)
            nextGen = nextGen :+ new Element(child1genes) :+ new Element(child2genes)
          }
          elements = nextGen
          elements.foreach(_.mutate(_mutationsOdds,allValues))
        }
        elements.foreach(_.setStrength(_strengthFunction))
        val totalStrength = elements.map(_.getStrength).sum
        elements.foreach(_.strengthToProbability(totalStrength))
        Sorting.quickSort[Element[T]](elements)(Ordering[Element[T]].reverse)
        if (elements.exists(el => el.getStrength.isPosInfinity)) break()
      }
    }
  }
}
