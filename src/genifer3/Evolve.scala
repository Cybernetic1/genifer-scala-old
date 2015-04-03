package genifer3

// Standard evolutionary algorithm:
// ==============================
// Initialize population
// Repeat until success:
//    Select parents
//    Recombine, mutate
//    Evaluate
//    Select survivors
// ==============================

// INPUT:
// 1) examples
// 2) goal / command
// 3) background knowledge (BK) in the form of logic formulas

// OUTPUT:
// logic formulas + program that achieve the goal

// POPULATION:
// contains logic formulas and program steps (actions)

// SCORING and EVALUATION:
// 1) correct answers from examples = reward
// 2) background knowledge may suggest "this is a good program"

// SELECTION / RECOMBINATION / MUTATION
// 1) Formulas / actions that have participated in successful examples
// 2) Sets of formulas / actions (program fragments) that are judged to be good by BK
// We should beware especially of synergistic interactions

// A central idea is to use BK to constrain the space of programs, but how?
// So there would be some facts, and the facts subsumes certain possibilities.
// An example:  knowing that the result would be a list, constrains the last steps
// of the program.  But this chain of reasoning remains mysterious...

// First we have to identify the programs that output lists.  This seems to be a halting
// problem.  If inference can say that a program is not good, would it be faster than if
// it is evaluated?  Also, we may have to distinguish between actions and whole programs.
// Does the population contain whole programs?  So even representing the programs is an
// unsolved problem.  But allowing lists to exist in the KB may be OK.

// But why can't a list of formulas be a formula?

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

// for graphics:
import scala.swing._
import scala.swing.event._
import java.awt.Color
import java.awt.Graphics2D

object Evolve {

  val r = new scala.util.Random

  // ** Initialize population
  // This is the set of current KB formulas, no need to initialize

  // Repeat until success:
  //    ** Select formulas to recombine
  //    ** Select formulas to mutate
  //    at this point we get some new candidates
  //    ** Evaluate new KB
  //        test KB on new / existing examples
  //    ** Select survivors
  //        based on scores

  // This is the example from the book "Clever Algorithms", translated from Ruby

  // count number of '1'
  def oneMax(bitString: String): Int = {
    bitString.count(_ == '1')
  }

  // generate a string of length num_bits
  def randomBitString(numBits: Int): String = {
    val s = new StringBuilder
    for (i <- 1 to numBits) {
      s.append(if (r.nextBoolean) '1' else '0')
    }
    print("new born: ")
    printCandidate(s.toString)
    s.toString
  }

  // pick 2 random but *distinct* candidates, pick the one with higher fitness
  def binaryTourament(pop: Array[String]): String = {
    val i = r.nextInt(pop.length)
    val j = r.nextInt(pop.length)

    //while (i == j)
      //j = r.nextInt(pop.length)

    if (fitness(pop(i)) > fitness(pop(j)))
      pop(i)
    else
      pop(j)
  }

  def fitness(dna : String): Int = {
    // println("fitness: " + dna)
    oneMax(dna)
  }

  // Randomly mutate DNA.
  // DNA's length = # of times to attempt mutation.
  // rate = 1/(DNA's length), so longer strand, lower mutation rate
  def pointMutation(dna: String, rate: Float = 1.0f / 64): String = {
    val result = new StringBuilder

    // print("in: "); printCandidate(dna)
    for (c <- dna) {
      result.append(if (r.nextFloat() < rate)
                      { if (c == '1') '0' else '1' }
                    else c)
    }
    // print("ou: "); printCandidate(result.toString())
    result.toString()
  }

  // Pick a point within Parent1,
  // cross Parent1's DNA with Parent2's
  def crossover(parent1: String, parent2: String, rate: Float): String = {
    if (r.nextFloat() > rate)
      return parent1

    val point = r.nextInt(parent1.length - 2) + 1
    val mix = parent1.substring(0, point) ++ parent2.substring(point, parent1.length)
    /*
    println("p1: " + parent1)
    println("mx: " + mix)
    println("p2: " + parent2)
    print  ("  : ")
    for (i <- 0 until point)
      print(" ")
    println("^\n")
    */
    mix
  }

  // Reproduce for 1 generation
  def reproduce(selected: Array[String], popSize: Int, crossRate: Float, mutationRate: Float): Array[String] = {
    var children = new ListBuffer[String]
    var p1, p2: String = null

    breakable { for (i <- 0 to selected.length) {
      p1 = selected(i)
      p2 = if ((i % 2) == 0) selected(i+1) else selected(i-1)
      if (i == selected.length - 1) p2 = selected(0)
      // p2 = selected(r.nextInt(selected.length))

      val child = crossover(p1, p2, crossRate)
      children += pointMutation(child, mutationRate)
      if (children.length >= popSize) break()
    }}
    // println(results.length)
    children.toArray
  }

  // Constant parameters:
  val numBits = 64
  val maxGens = 100
  val popSize = 100
  val crossRate: Float = 0.98f
  val mutationRate: Float = 1.0f / numBits

  var $pop = new Array[String](popSize)

  // Main algorithm for genetic search
  def evolve(): Unit = {
    // initialize population
    var population = new Array[String](popSize)
    population = Array.fill(popSize)(randomBitString(numBits))

    val population2 = population.sortBy(fitness).reverse
    for (c <- population2) {
      print("init: ")
      printCandidate(c)
    }

    // var selected = new Array[String](popSize)
    var best: String = ""

    breakable { for (i <- 0 to maxGens) {
      print(f"gen $i%03d, ")
      val selected = Array.fill(popSize)(binaryTourament(population))
      // for (c <- selected) { print("select: ") printCandidate(c) }
      val selected2 = selected.sortBy(fitness).reverse
      val gen2 = reproduce(selected2, popSize, crossRate, mutationRate)
      // println("# children = " + children.length)
      // println("\n Sorting....\n")
      val gen2s = gen2.sortBy(fitness).reverse
      // for (c <- children) { print("child: ");  printCandidate(c) }
      // println("Sorted....")

      if (fitness(gen2s(0)) >= fitness(best))
        best = gen2s(0)

      gen2s.copyToArray(population)
      $pop = population
      frame.repaint()
      println("best: " + fitness(best) + " " + best)

      if (fitness(best) == numBits) {
        println("Success!!!")
        break()
      }

      Thread.sleep(500)
      // System.in.read()
    }}

  System.in.read()

  }

  def printCandidate(c: String): Unit = {
    println(c + " <" + fitness(c) + ">")
  }

  // point where user pressed mouse button (-1 for none)
  var startPoint = new Point(-1, -1)
  // point where user released mouse button (-1 for none)
  var endPoint = new Point(-1, -1)
  // current "drag" point (-1 for none)
  var dragPoint = new Point(-1, -1)

  // panel for drawing
  val drawingPanel = new Panel {

    override def paint(g : Graphics2D) {
      g.setPaint(Color.BLACK)
      // clear background
      g.fill(new Rectangle(0,0,size.width,size.height))

      g.setPaint(Color.GREEN)

      var y = 5
      for (p <- $pop) {
        var x = 5
        for (c <- p) {
          if (c == '1')
            g.fill(new Rectangle(x, y, 4, 4))
          x += 5
        }
        y += 5
      }

      // System.in.read()
      /*
      if ((startPoint.x != -1) && (endPoint.x != -1)) {
        g.fill(pointsToRectangle(startPoint, endPoint))
      }
      // draw outline of rectangle in progress
      if ((startPoint.x != -1) && (dragPoint.x != -1)) {
        g.draw(pointsToRectangle(startPoint, dragPoint))
      }
      */
    }

    listenTo(mouse.clicks,mouse.moves,keys)

    reactions += {
      case e:MousePressed =>
        requestFocus() // only needed because we have keyboard input too
        startPoint = e.point
        endPoint = new Point(-1, -1)
        dragPoint = new Point(-1, -1)
        repaint()
      case e:MouseReleased =>
        requestFocus() // only needed because we have keyboard input too
        endPoint = e.point
        dragPoint = new Point(-1, -1)
        repaint()
      case e:MouseDragged =>
        requestFocus() // only needed because we have keyboard input too
        dragPoint = e.point
        repaint()
      case e:KeyTyped =>
        requestFocus() // only needed because we have keyboard input too
        repaint()
        /*
        if (e.char == 'c') {
          startPoint = new Point(-1, -1)
          endPoint = new Point(-1, -1)
          dragPoint = new Point(-1, -1)
        }
        repaint()
        */
    }
  }

  // main layout
  val frame = new MainFrame {
    title = "Population"
    contents = new BorderPanel {
      layout += (drawingPanel -> BorderPanel.Position.Center)
    }
    size = new Dimension(340,560)
  }

  // start up
  frame.visible = true
}
