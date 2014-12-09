// To-do:
// -- unify
// -- map jug to KB
// -- reduce
// add 2 key Formulas to KB

package genifer3

import scala.io.Source
import scala.collection.JavaConversions._

object Genifer {

	var kb = new KB()

	def main(args: Array[String]) {
		println("This is Genifer.")

		// load unification algorithm from Clojure
		new clojure().initClojure()

		// Read KB from file
		val filename = "test/Cantonese-Mandarin-dictionary.txt"
		for(line <- Source.fromFile(filename).getLines()) {
			println(line)
			var list = new java.util.ArrayList[Atom]
			for(term <- line.split(" ")) {
				if (term(0) == '\"')
					list += new Atom(term.replace("\"", ""))
				else
					list += new Atom(term.toInt)
			}
			kb.addFormula(new Formula(list))
		}
	}

	// ***** Convert Mandarin Chinese string to Cantonese
	// This will be called by Genifer-server
	// INPUT:  string to be Cantonized
	def cantonize(str: String): String = {
		val mapReduce = new MapReduce()

		// for each command, the jug is the single goal
		// command = goal = Formula
		var list = new java.util.ArrayList[Atom]
		list += new Atom(1001)
		for(char <- str) {
				list += new Atom(char.toString)
		}
		val command = new Formula(list)

		// jug should be a list of Formulas, in this case just 1
		val mesh = mapReduce.map(kb, List(command))

		// matching returns the resulting 'graph' or mesh
		val answer = mapReduce.reduce(mesh)
		mapReduce.action(answer)
	}
}
