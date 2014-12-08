// To-do:
// -- map jug to KB
// -- need to have rule structure = precond + postcond
// -- reduce
// add 2 key Formulas to KB

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
					list += new Atom(Right(term.replace("\"", "")))
				else
					list += new Atom(Left(term.toInt))
			}
			kb.addFormula(new Formula(list.toList))
		}
	}

	// ***** Convert Mandarin Chinese string to Cantonese
	// This will be called by Genifer-server
	// INPUT:  string to be Cantonized
	def cantonize(str: String): String = {
		val mapReduce = new MapReduce()

		// for each command, the jug is the single goal
		// command = goal = Formula
		var list = new ListBuffer[Atom]()
		list += new Atom(Left(1001))
		for(char <- str) {
				list += new Atom(Right(char.toString))
		}
		val command = new Formula(list.toList)

		// jug should be a list of Formulas, in this case just 1
		val mesh = mapReduce.map(kb, List(command))

		// matching returns the resulting 'graph' or mesh
		val answer = mapReduce.reduce(mesh)
		mapReduce.action(answer)
	}
}
