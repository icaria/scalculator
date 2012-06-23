package patternMatchCalculator
 	import scala.collection.mutable.HashMap
 	
    object calc {
      
        var myVar : Double = 0
  
        //setup classes
		abstract class Expr
		abstract class UnaryExpr(val exp:Expr) extends Expr
		abstract class BinaryExpr(val left:Expr, val right:Expr) extends Expr
		
    	case class Number(val value:Double) extends Expr
		case class Variable(val value:String) extends Expr
        
		case class NegationExpr(override val exp:Expr ) extends UnaryExpr (exp)
		case class BracketExpr(override val exp:Expr ) extends UnaryExpr (exp)
		
		case class AddExpr(override val left:Expr, override val right:Expr ) extends BinaryExpr (left,right)
	    case class SubtExpr(override val left:Expr, override val right:Expr ) extends BinaryExpr (left,right)
		case class MultExpr(override val left:Expr, override val right:Expr ) extends BinaryExpr (left,right)
		case class DivExpr(override val left:Expr, override val right:Expr ) extends BinaryExpr (left,right)
		
		def eval(exp:Expr) : Double = {
			exp match {
				case Number(v) => v
				// case Variable(v) => myVar
				case BracketExpr(exp) => eval(exp)
				case NegationExpr(exp) => -(eval(exp))
				case AddExpr(left, right) => eval(left) + eval(right)
				case SubtExpr(left, right) => eval(left) - eval(right)
				case MultExpr(left, right) => eval(left) * eval(right)
				case DivExpr(left, right) => eval(left) / eval(right)
			}
		}
		
		def simplify(e : Expr) : Expr =
	    {
	      val simplifiedExpr = e match{
	        case NegationExpr(x) => NegationExpr(simplify(x))
	        case BracketExpr(x) => BracketExpr(simplify(x))
	      	case AddExpr(x,y) => AddExpr(simplify(x), simplify(y))
	        case SubtExpr(x,y) => SubtExpr(simplify(x), simplify(y))
	        case MultExpr(x,y) => MultExpr(simplify(x), simplify(y))
	        case DivExpr(x,y) => DivExpr(simplify(x), simplify(y))
	        case _ => e
	      }
		  
		  simplifiedExpr match {
	        //When adding 0 to an expression, return expression
	        case AddExpr(Variable(v),Number(0)) => Variable(v)
	        case AddExpr(Number(0),Variable(v)) => Variable(v)
	        
	        //If x - x, return 0
	        case SubtExpr( Variable(v),Variable(x) ) if v == x => Number(0)
	        
	        //If x + x
	        case AddExpr( Variable(v),Variable(x) ) if v == x => MultExpr(Number(2),Variable(v))
	        
	        //if x * 1 return x
	        case MultExpr(Variable(v),Number(1)) => Variable(v)
	        case MultExpr(Number(1),Variable(v)) => Variable(v)
	        
	        //if x / 1 return x
	        case DivExpr(Variable(v),Number(1)) => Variable(v)
	        
	        //If (e*x)/x return e
	        case DivExpr(MultExpr(left,Variable(v)),Variable(x)) if v == x => simplify(left)
	        case DivExpr(MultExpr(Variable(v),left),Variable(x)) if v == x => simplify(left)
	        
	        //if (e1*x)+(e2*x) return (e1+e2)*x
	        case AddExpr(MultExpr(e1,Variable(x)),MultExpr(e2,Variable(t))) if x == t => 
	          MultExpr(AddExpr(simplify(e1),simplify(e2)), Variable(t) )  
	        case AddExpr(MultExpr(e1,Variable(x)),MultExpr(Variable(t),e2)) if x == t => 
	          MultExpr(AddExpr(simplify(e1),simplify(e2)), Variable(t) )  
	        case AddExpr(MultExpr(Variable(x),e1),MultExpr(e2,Variable(t))) if x == t => 
	          MultExpr(AddExpr(simplify(e1),simplify(e2)), Variable(t) )  
	        case AddExpr(MultExpr(Variable(x),e1),MultExpr(Variable(t),e2)) if x == t => 
	          MultExpr(AddExpr(simplify(e1),simplify(e2)), Variable(t) )  

	        case _ => e
	      }
	    }
		
		def replace(e:Expr, map:HashMap[String,Double]) : Expr =
		{
			e match {
			  case Variable(v) if map.contains(v) => Number(map.getOrElse(v, 0))
			  case NegationExpr(x) => NegationExpr(replace(x, map))
			  case BracketExpr(x) => BracketExpr(replace(x, map))
			  case AddExpr(x,y) => AddExpr(replace(x, map), replace(y, map))
			  case SubtExpr(x,y) => SubtExpr(replace(x, map), replace(y, map))
			  case MultExpr(x,y) => MultExpr(replace(x, map), replace(y, map))
			  case DivExpr(x,y) => DivExpr(replace(x, map), replace(y, map))
			  case _ => e
			}
		}
		
		def main(args: Array[String]) {
		    val map = new HashMap[String, Double]
		    map += "x" -> 5
		    map += "y" -> 4
		  	val exp = new MultExpr (AddExpr(Variable("x"), Number(4)) , Variable("y"))
		    println(eval(simplify(replace(exp, map))))
		}
	}
