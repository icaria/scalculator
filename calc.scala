package Calculator {
	var exp = "4+6="

	object Calculator {
		def main(args: Array[String]) {
			println("This is a calculator")
		}
	}
	
	abstract class Operation {
		def Operation(first: Int, second: Int)
	}
	
	class ParseExpression {
		
	}
	
	class Addition extends Operation {
		var x;
		var y;
		def Addition() {
			
		}
	}
	
	class Subtraction extends Operation {
		var x;
		var y;
		def Subtraction() {
			
		}
	}
	
	class Multiplication extends Operation {
		def Multiplication() {
			
		}
	}
	
	class Division extends Operation {
		def Division() {
			
		}
	}
}
