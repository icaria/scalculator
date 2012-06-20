package Calculator {
	//var exp = "4+6="

	object Calculator {
		def main(args: Array[String]) {
			println("This is a calculator")
		}
	}

	abstract class Expression {
		case class Number(value: Int) extends Expression
		case class UnaryOp(operator: String, arg: Expression) extends Expression
		case class BinaryOp(operator: String, left: Expression, right: Expression) extends Expression
	}
	
}
