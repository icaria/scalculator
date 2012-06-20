package Calculator {
	//var exp = "4+6="

	object Calculator {
		def main(args: Array[String]) {
			println("This is a calculator")
		}
	}

	class Expression {
		var exp
		Expression 	
	}


	
	abstract class Operation {
		def compute(x: Int, y: Int)
	}
	
	class Addition extends Operation {
		def Addition() {
		}

		def compute(x: Int, y: Int) {
			return x + y
		}
	}
	
	class Subtraction extends Operation {
		def Subtraction() {
			
		}

		def compute(x: Int, y: Int) {
			return x - y
		}
	}
	
	class Multiplication extends Operation {
		def Multiplication() {
			
		}

		def compute(x: Int, y: Int) {
			return x * y
		}
	}
	
	class Division extends Operation {
		def Division() {
			
		}

		def compute(x: Int, y: Int) {
			if(y == 0) {
				println("Nuclear Launch (Division by zero) detected.")
				return -1
			}
			return x / y
		}
	}
}
