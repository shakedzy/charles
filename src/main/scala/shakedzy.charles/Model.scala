package shakedzy.charles

import scala.util.Sorting
import scala.math._
import scala.util.control.Breaks._

/** A Genetic Model. The model accepts a population and certain predefined parameters, by which it will let
  * the population evolve, as it tries to reach perfection.
  *
  * @constructor create a new Genetic Model
  * @param population a sequence of subjects (sometimes refer to as Chromosomes in Genetic Model's terminology),
  *                   where each subject is a sequence of genes, each of type T
  * @param allValues a sequence of all possible values any single gene can have
  * @param strengthFunction a function that maps a subject in the population to a non-negative number from 0 to Inf,
  *                         which represents its strength, and therefore it probability to survive and reproduce. The
  *                         higher the number, the stronger the subject is
  * @param offspringFunction a reproduction function that maps two subjects to two new subjects. This is the
  *                          definition of the reproduction mechanism works, and how to create the offspring of
  *                          two subjects in the population
  * @param elitismRatio a continuous number in the range [0,1], representing the percentage of elitists in each
  *                     generation. Elitists are the strongest subject in their generation, and therefore survive and
  *                     advance untouched to the next generation (mutation can still apply)
  * @param mutationsOdds a continuous number in the range [0,1], which determines the probability for mutation of
  *                      the subjects in each generation. A mutation is a single binary bit in the subject's genes
  *                      being randomly flipped (subjects are transformed to binary representation behind the scenes)
  * @param generations a non-negative integer, which determines the number of iterations the model will do
  * @param seed a seed to be supplied to the model's pseudo-random number generator
  * @tparam T the type of a single gene of each subject in the population
  */
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

  /** This function removes all Elements of the population with strength 0, as they have no chance of
    * reproduce or survive
    */
  protected def killMisfits(): Unit = elements = elements.filter(_.getStrength > 0.0)

  /** This function randomly selects a single Element of a population based on their strength.
    * An ordered population is prefered for performance, where the first Element of the population has
    * the highest probability of survival and reproduction, and the rest are ordered by a decreasing
    * order of their strength (and survival probability).
    *
    * @param ignoreThisElement if defined, this Element will not participate in the random selection
    * @return a random Element
    */
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

  /** Reset the population to the initial population
    */
  def reset(): Unit = setPopulation(population)

  /** Returns the strongest subject in the population
    *
    * @return a sequence of genes (values of type T)
    */
  def getBest: Seq[T] = elements.head.getGenes

  /** Returns the n strongest subject in the population
    *
    * @param n how many subjects should be returned
    * @return a sequence of subjects, each itself a sequence of genes (values of type T)
    */
  def getBest(n: Int): Seq[Seq[T]] = elements.take(n).map(_.getGenes)

  /** The model's main procedure. This starts the evolution of the subjects of the population
    * for the specified amount of generations. This includes reproduction, elitists survival
    * and mutation.
    */
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
