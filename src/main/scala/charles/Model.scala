package charles

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
  * @param duplicationPolicy defines how the model should treat the case of duplicates of the same subject in the
  *                          population in each generation. Can be either "ignore" (leave duplicates in the population),
  *                          "kill" (remove duplicates, number of subjects in the population will decrease) or "replace"
  *                          (duplicates will bre replaced with new Elements created in the same way as reproducing).
  *                          The "replace" option can also be "replace:x" where x is a positive integer, defining the
  *                          number of attempts the model will try to replace duplicates with new Elements. In any case,
  *                          The size of the population will not change when using the "replace" policy.
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
               duplicationPolicy: String = "ignore",
               seed: Long = System.currentTimeMillis()) {

  private val defaultEndReason = (-1, null)
  private val defaultDuplicationReplaceAttempts = 3
  protected implicit val random: Random = new Random

  protected var _strengthFunction: Seq[T] => Double = _
  protected var _offspringFunction: (Seq[T],Seq[T]) => (Seq[T],Seq[T]) = _
  protected var _elitismRatio: Double = _
  protected var _mutationsOdds: Double = _
  protected var _generations: Int = _
  protected var _duplicationPolicy: String = _
  protected var _seed: Long = _
  protected var elements: Array[Element[T]] = _
  protected var endReason: (Int, String) = defaultEndReason
  protected var currentGeneration: Int = 0
  protected var duplicationReplaceAttempts: Int = defaultDuplicationReplaceAttempts

  setStrengthFunction(strengthFunction)
  setOffspringFunction(offspringFunction)
  setElitismRatio(elitismRatio)
  setMutationOdds(mutationsOdds)
  setGenerations(generations)
  setDuplicationPolicy(duplicationPolicy)
  setSeed(seed)
  setPopulation(population)

  def setStrengthFunction(strengthFunction: Seq[T] => Double): Unit = _strengthFunction = strengthFunction
  def setOffspringFunction(offspringFunction: (Seq[T],Seq[T]) => (Seq[T],Seq[T])): Unit = _offspringFunction = offspringFunction
  def setDuplicationPolicy(duplicationPolicy: String): Unit = {
    val dp = duplicationPolicy.toLowerCase
    dp match {
      case "ignore" | "kill" | "replace" =>
        _duplicationPolicy = dp
        duplicationReplaceAttempts = defaultDuplicationReplaceAttempts
      case policy if policy.startsWith("replace:") =>
        _duplicationPolicy = "replace"
        duplicationReplaceAttempts = try {
          val att = policy.split(":")(1).toInt
          if (att < 1) throw new RuntimeException("")
          else att
        } catch {case _: Exception => throw new RuntimeException("Invalid number of attempts!")}
      case _ => throw new RuntimeException("Invalid duplication policy!")
    }
  }
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
  def getEndReason: (Int, String) = endReason
  def getCurrentGeneration: Int = currentGeneration
  def getDuplicationPolicy: String =
    if (_duplicationPolicy == "replace") s"${_duplicationPolicy}:$duplicationReplaceAttempts"
    else _duplicationPolicy

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
  def reset(): Unit = {
    setPopulation(population)
    endReason = defaultEndReason
    currentGeneration = 0
  }

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

  /** This function handles the case of duplicate Elements in the population according
    * to the selected policy.
    */
  private def handleDuplicates(): Unit = {
    _duplicationPolicy match {
      case "kill" => elements = elements.distinct
      case "ignore" => Unit //do nothing
      case "replace" =>
        breakable {
          for (_ <- Range(0,duplicationReplaceAttempts)) {
            val n = elements.length
            elements = elements.distinct
            val missing = n - elements.length
            if (missing == 0) break()
            val numberOfCouples = ceil(missing/2).toInt
            val newElements = breed(numberOfCouples).drop(missing % 2)
            elements = elements ++ newElements
          }
        }
    }
  }

  /** This function is responsible for creating a pair of new Elements based on the number of pairs
    * requested. The model's offspringFunction is used for the creation of new Elements.
    *
    * @param numberOfCouples the number of pairs of new Elements to create
    * @return a sequence of the new Elements created
    */
  private def breed(numberOfCouples: Int): Seq[Element[T]] = {
    var elements = Seq.empty[Element[T]]
    for (_ <- Range(0,numberOfCouples)) {
      val father = selectElement()
      val mother = selectElement(ignoreThisElement = Option(father))
      val (child1genes, child2genes) = _offspringFunction(father.getGenes,mother.getGenes)
      elements = elements :+ new Element(child1genes) :+ new Element(child2genes)
    }
    elements
  }

  /** The model's main procedure. This starts the evolution of the subjects of the population
    * for the specified amount of generations. This includes reproduction, elitists survival
    * and mutation.
    */
  def evolve(): Unit = {
    endReason = defaultEndReason
    breakable {
      for (g <- Range(0,_generations+1)) {
        currentGeneration = g
        if (g > 0) {
          val elNum = elements.length
          killMisfits()
          if (elements.length < 2) {
            endReason = (2, "Population perished")
            break()
          }
          val elitistsNum = round(_elitismRatio * elNum).toInt
          val elitists = elements.take(elitistsNum)
          val remainingCouplesNum = floor((elNum - elitistsNum)/2).toInt
          elements = elitists ++ breed(remainingCouplesNum)
          handleDuplicates()
          elements.foreach(_.mutate(_mutationsOdds,allValues))
        }
        elements.foreach(_.setStrength(_strengthFunction))
        val totalStrength = elements.map(_.getStrength).sum
        elements.foreach(_.strengthToProbability(totalStrength))
        Sorting.quickSort[Element[T]](elements)(Ordering[Element[T]].reverse)
        if (elements.exists(el => el.getStrength.isPosInfinity)) {
          endReason = (1, "Ideal solution found")
          break()
        }
      }
    }
    if (endReason == defaultEndReason) endReason = (0, "Evolution completed")
  }
}
