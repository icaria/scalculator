object Calculator {
      
  var myVar : Double = 0

      //setup classes
	abstract class Expr
	abstract class BinaryExpr(val left:Expr, val right:Expr) extends Expr
	abstract class UnaryExpr(val exp:Expr) extends Expr
	abstract class NumberExpression(val value:Double) extends Expr
  case class AddExpr(override val left:Expr, override val right:Expr ) extends BinaryExpr (left,right)
  case class SubtExpr(override val left:Expr, override val right:Expr ) extends BinaryExpr (left,right)
	case class MultExpr(override val left:Expr, override val right:Expr ) extends BinaryExpr (left,right)
	case class DivExpr(override val left:Expr, override val right:Expr ) extends BinaryExpr (left,right)
	case class NegationExpr(override val exp:Expr ) extends UnaryExpr (exp)
	case class BracketExpr(override val exp:Expr ) extends UnaryExpr (exp)
	case class Number(override val value: Double) extends NumberExpression (value)
	case class Variable(override val value:Double) extends NumberExpression (value)
      
	def eval(exp:Expr) : Double = {
		simplify(exp) match {
			case Number(v) => v
			case Variable(v) => myVar
			case AddExpr(left, right) => eval(left) + eval(right)
			case SubtExpr(left, right) => eval(left) - eval(right)
			case MultExpr(left, right) => eval(left) * eval(right)
			case DivExpr(left, right) => eval(left) / eval(right)
			case NegationExpr(exp) => -eval(exp)
			case BracketExpr(exp) => eval(exp)
		}
	}
	
	def simplify(e : Expr) : Expr =
    {
      e match {
        //When adding 0 to an expression, return expression
        case AddExpr(Variable(v),Number(0)) => Variable(v)
        case AddExpr(Number(0),Variable(v)) => Variable(v)
        //If x - x, return 0
        case SubtExpr( Variable(v),Variable(x) ) => Number(0)
        //If x + x
        case AddExpr( Variable(v),Variable(x) ) => MultExpr(Number(2),Variable(v))
        //if x * 1 return x
        case MultExpr(Variable(v),Number(1)) => Variable(v)
        case MultExpr(Number(1),Variable(v)) => Variable(v)
        //if x / 1 return x
        case DivExpr(Variable(v),Number(1)) => Variable(v)
        //If (e*x)/x return e
        case DivExpr(MultExpr(left,Variable(v)),Variable(x))  => left
        
        //if (e1*x)+(e2*x) return (e1+e2)*x
        case AddExpr(MultExpr(e1,Variable(_)),MultExpr(e2,Variable(t)))  => MultExpr(AddExpr(e1,e2), Variable(t) )  
        case AddExpr(MultExpr(Variable(_),e1),MultExpr(Variable(v),e2))  => MultExpr(AddExpr(e1,e2), Variable(v) )  

        case _ => e
      }
    }
	

	def main(args: Array[String]) {
	    println("Nuclear Launch Detected.")

	    val x = eval( new Number(5) )
	    println(x)
	}
	
	
}
