// To-do:
// - map jar to KB

package genifer3

import scala.io.Source
import scala.collection.mutable.ListBuffer

object Genifer {

	var kb = new KB()

	def main(args: Array[String]) {
		println("This is Genifer.")

		// Read KB from file
		val filename = "/home/yky/Cantonese-Mandarin-dictionary.txt"
		for(line <- Source.fromFile(filename).getLines()) {
			println(line)
			var list = new ListBuffer[Atom]()
			for(term <- line.split(" ")) {
				if (term(0) == '\"')
					list += new Atom(Right(term))
				else
					list += new Atom(Left(term.toInt))
			}
			// Add Formula to KB
			kb.addFormula(new Formula(list.toList))
		}
	}

	// ***** Convert Mandarin Chinese string to Cantonese
	// This will be called by Genifer-server
	def cantonize(s: String): String = {
		val mapReduce = new MapReduce()
		// Prepare jug, execute jug
		// for each command, the jug is the single goal
		// command = goal = formula
		val command = new Formula(List())
		// after creating jug, send the jug to matching
		// jug should be a list of formulas
		val mesh = mapReduce.mapKB(List(command))
		// matching returns the resulting 'graph'
		val answer = mapReduce.reduce(mesh)
		mapReduce.action(answer)
	}
}
