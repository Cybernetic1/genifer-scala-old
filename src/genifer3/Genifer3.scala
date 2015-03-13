// To-do:
// -- Cantonese test
//	* what is the program?
// -- learning module

/*================= Implementation Notes ===================
现在我们只有 KB。  KB 是一个序列； 那很好办。
而且可以从序列的尾部 轻易地做到「遗忘」的功能。
问题是有一些相对地恒久的事实。
==========================================================*/

package genifer3

import scala.io.Source

object Genifer3 {

	var kb = new KB()
  var wmem = new WorkingMemory()

  // val nullAtom = new Atom(0)    // a "global" constant to signify null

	def main(args: Array[String]) {
		println("This is Genifer.")

		// Read KB from file

		val filename = "/home/yky/scala/genifer3/test/Cantonese-Mandarin-dictionary.txt"		// for testing
		for(line <- Source.fromFile(filename).getLines()) {
			if (line(0) != ';') {
				println(line)
				var isEq: Boolean = false
				var list: List[Atom] = List()
				val t1 = new ∏

				for (term <- line.split(" ")) {
					if (term(0) == '\"')
						list ::= new Atom(term.replace("\"", ""))
					else if (term == "=") {
						isEq = true
						t1.atoms = list
						list = List()
					}
					else
						list ::= new Atom(term.toInt)
				}

				if (isEq) {
					val t2 = new ∏
					t2.atoms = list
					kb.addFormula(new Equation(t1, t2))
				}
				else {
					val f2 = new ∏
					f2.atoms = list
					kb.addFormula(f2)
				}
			}
		}

		// Alternative code (old):
		//		Source.fromFile(filename).getLines().foreach( line =>
		//			{
		//			println(line)
		//			val list = line.split(" ").toList.map { term =>
		//				if (term(0) == '\"')
		//					new Atom(term.replace("\"", ""))
		//				else
		//					new Atom(term.toInt)
		//				}
		//
		//			val f = new Formula()
		//			f.atoms = list
		//			kb.addFormula(f)
		//			})

		println("Formulas read into KB.\n\n")

		println(cantonize("什么"))

		println("\n\nSecond task:\n\n")
		println(cantonize("你喜欢做什么？"))
	}

	// ***** Convert Mandarin Chinese string to Cantonese
  // This is an example where we apply Map Reduce to a small task.
  // The "program" is:
  // 1. Initially, the In-Tape contains the sentence to be translated
  // 2. Focus on the In-Tape's first position
  // 3. ("Focus" automatically causes the word to be recognized)
  // 4. Cantonize the recognized word (which is now in Working Memory)
  // 5. Extract the word from Working Memory to Out-Tape
  // 6. Repeat from step #2
  // The Map Reduce processing is as follows:
  // 1. Initially, Tape2 contains the program
  // 2. Map Reduce is called, ie, Jug is applied to KB
  // 3. The
	// This function will be called by Genifer-server.
	// INPUT:  string to be Cantonized.
  /*================= Implementation Notes ===================
  程式应该要能储存在 tape 上。
  但我们要「执行」那 tape，但似乎「顺序执行」也是一个复杂过程。
  但如果连顺序执行的能力也没有，则又变成鸡生蛋问题。
  问题是「顺序执行」似乎不是一个原子动作，因为需要有中断的能力。
  Another way to do program sequences is via conditionals.
  But it is still more natural to have programs stored on tapes.
      
  ==========================================================*/

	def cantonize(str: String): String = {
		val mapReduce = new MapReduce()

		// for each command, the jug is the single goal
		// command = goal = Formula
		var list : List[Atom] =  List()
		// for(char <- str) {
		//		list :+= new Atom(char.toString)
		// }
		list :+= new Atom(str)
		list :+= new Atom(1000)

		println("list atoms: ", list)
		val command1 = new ∏(list)
		println("term = ", command1.toString)
		val command = new ∏
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
