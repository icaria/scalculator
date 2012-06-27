package expAbstractData
import scala.collection.mutable.HashMap

/**
 This trait will extend all data types with replace function.
 replace function will replace function values with mapped constant values when 
 the expression is being evaluated
 */
trait ReplaceAllOperations extends BaseAllOperations{
  type exp <: Exp
  
  trait Exp extends super.Exp {
    def replace(map:HashMap[String,Double]): exp
  }
  
  trait Num extends super.Num with Exp {
    def replace(map:HashMap[String,Double]) = Num (value) 
  }
  trait Var extends super.Var with Exp {
    def replace(map:HashMap[String,Double]): exp = {
      if (map.contains(symbol)) Num(map.getOrElse(symbol, 0)) else Var(symbol)
    } 
  }
  trait Neg extends super.Neg with Exp{
    def replace(map:HashMap[String,Double]) = Neg(operand.replace(map))
  }
  trait Bracket extends super.Bracket with Exp{
    def replace(map:HashMap[String,Double]) = Bracket(operand.replace(map))
  }
  
  trait Plus extends super.Plus with Exp{
    def replace(map:HashMap[String,Double]) = Plus(left.replace(map), right.replace(map))
  }
  
  trait Subt extends super.Subt with Exp{
    def replace(map:HashMap[String,Double]) = Subt(left.replace(map), right.replace(map))
  }

  trait Mult extends super.Mult with Exp{
    def replace(map:HashMap[String,Double]) = Mult(left.replace(map), right.replace(map))
  }

  trait Div extends super.Div with Exp{
    def replace(map:HashMap[String,Double]) = Div(left.replace(map), right.replace(map))
  }
}
