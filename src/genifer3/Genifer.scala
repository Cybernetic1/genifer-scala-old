// To-do:
// -- map jug to KB
// -- reduce
// add 2 key Formulas to KB

package genifer3

import java.util

import scala.io.Source
import scala.collection.JavaConversions._

object Genifer {

	var kb = new KB()

	def main(args: Array[String]) {
		println("This is Genifer.")

		// Read KB from file
		val filename = "test/Cantonese-Mandarin-dictionary.txt"
		for(line <- Source.fromFile(filename).getLines()) {
			println(line)
			var list = new util.LinkedList[Atom]
			for(term <- line.split(" ")) {
				if (term(0) == '\"')
					list += new Atom(term.replace("\"", ""))
				else
					list += new Atom(term.toInt)
			}
			val f = new Formula()
			f.atoms = list
			kb.addFormula(f)
		}

		println("Formulas read into KB.")

		// println(cantonize("你锺意做乜？"))
		println(cantonize("乜"))
	}

	// ***** Convert Mandarin Chinese string to Cantonese
	// This will be called by Genifer-server
	// INPUT:  string to be Cantonized
	def cantonize(str: String): String = {
		val mapReduce = new MapReduce()

		// for each command, the jug is the single goal
		// command = goal = Formula
		var list = new util.LinkedList[Atom]
		list += new Atom(1000)
		list += new Atom(-1)
		for(char <- str) {
				list += new Atom(char.toString)
		}
		println("list atoms: ", list)
		val command1 = new ∏(list)
		println("formula atoms: ", command1.atoms)
		val command = new Formula()
		command.atoms = list
		println("command atoms: ", command.atoms)

		// jug should be a list of Formulas, in this case just 1
		val jug = List(command)
		for (jugItem <- jug)
			println("jug[i] atoms: ", jugItem.atoms)
		val mesh = mapReduce.map(kb, jug)

		// matching returns the resulting 'graph' or mesh
		val answer = mapReduce.reduce(mesh)
		mapReduce.action(answer)
	}
}
