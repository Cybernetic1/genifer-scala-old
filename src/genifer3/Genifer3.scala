// To-do:
// --
// -- add 2 key Formulas to KB

package genifer3

import scala.io.Source

object Genifer3 {

	var kb = new KB()

	def main(args: Array[String]) {
		println("This is Genifer.")

		// Read KB from file
		val filename = "/home/yky/scala-workplace/genifer3/test/Cantonese-Mandarin-dictionary.txt"		// for testing
		Source.fromFile(filename).getLines().foreach( line =>
			{
			println(line)
			val list = line.split(" ").toList.map { term =>
				if (term(0) == '\"')
					new Atom(term.replace("\"", ""))
				else
					new Atom(term.toInt)
				}

			val f = new Formula()
			f.atoms = list
			kb.addFormula(f)
			})

		// Alternatively:
		//		for(line <- Source.fromFile(filename).getLines()) {
		//			println(line)
		//			var list: List[Atom] = List()
		//			for(term <- line.split(" ")) {
		//				if (term(0) == '\"')
		//					list ::= new Atom(term.replace("\"", ""))
		//				else
		//					list ::= new Atom(term.toInt)
		//			}
		//			val f = new Formula()
		//			f.atoms = list
		//			kb.addFormula(f)
		//		}

		println("Formulas read into KB.")

		println(cantonize("乜"))
		println(cantonize("你锺意做乜？"))
	}

	// ***** Convert Mandarin Chinese string to Cantonese
	// This will be called by Genifer-server
	// INPUT:  string to be Cantonized
	def cantonize(str: String): String = {
		val mapReduce = new MapReduce()

		// for each command, the jug is the single goal
		// command = goal = Formula
		var list : List[Atom] =  List()
		list :+= new Atom(1000)
		list :+= new Atom(-1)
		for(char <- str) {
				list :+= new Atom(char.toString)
		}
		println("list atoms: ", list)
		val command1 = new ∏(list)
		println("formula = ", command1.toString)
		val command = new Formula()
		command.atoms = list
		println("command = ", command.toString)

		// jug should be a list of Formulas, in this case just 1
		val jug = List(command)
		for (jugItem <- jug)
			println("jug[i] = ", jugItem.toString)
		val mesh = mapReduce.map(kb, jug)

		// matching returns the resulting 'graph' or mesh
		val answer = mapReduce.reduce(mesh)
		if (answer != null)
			mapReduce.action(answer)
		else
			null
	}
}
