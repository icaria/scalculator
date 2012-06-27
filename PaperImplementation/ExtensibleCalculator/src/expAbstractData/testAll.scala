package expAbstractData

import scala.collection.mutable.HashMap

/**Combine all extended features extended in other files
 * */
 
trait ShowSimplifyReplaceAllOperations extends ShowAllOperations with SimplifyAllOperations with ReplaceAllOperations
{
  type exp <: Exp
  trait Exp extends super[ShowAllOperations].Exp with super[SimplifyAllOperations].Exp with super[ReplaceAllOperations].Exp
  trait Num extends super[ShowAllOperations].Num with super[SimplifyAllOperations].Num with super[ReplaceAllOperations].Num with Exp
  trait Var extends super[ShowAllOperations].Var with super[SimplifyAllOperations].Var with super[ReplaceAllOperations].Var with Exp
  trait Bracket extends super[ShowAllOperations].Bracket with super[SimplifyAllOperations].Bracket with super[ReplaceAllOperations].Bracket with Exp
  trait Neg extends super[ShowAllOperations].Neg with super[SimplifyAllOperations].Neg with super[ReplaceAllOperations].Neg with Exp
  trait Plus extends super[ShowAllOperations].Plus with super[SimplifyAllOperations].Plus with super[ReplaceAllOperations].Plus with Exp
  trait Subt extends super[ShowAllOperations].Subt with super[SimplifyAllOperations].Subt with super[ReplaceAllOperations].Subt with Exp
  trait Mult extends super[ShowAllOperations].Mult with super[SimplifyAllOperations].Mult with super[ReplaceAllOperations].Mult with Exp
  trait Div extends super[ShowAllOperations].Div with super[SimplifyAllOperations].Div with super[ReplaceAllOperations].Div with Exp
}


/** Testing the resulting combination
 */
object testAll extends ShowSimplifyReplaceAllOperations with Application
{
  type exp = Exp
  /**Creating data instances
   */
  def Num(v: Double) = new Num {value = v}
  def Var(s: String) = new Var {symbol = s}
  def Plus(l: exp, r: exp) = new Plus { left = l; right = r}
  def Subt(l: exp, r: exp) = new Subt { left = l; right = r}
  def Mult(l: exp, r: exp) = new Mult { left = l; right = r}
  def Div(l: exp, r: exp) = new Div { left = l; right = r}
  def Bracket(o: exp) = new Bracket { operand = o }
  def Neg(t: exp) = new Neg { operand = t}
  
  var term = new Plus{
     left = new Num { value = 1.0 }
     right = new Bracket { 
    	 operand = new Plus {
    		 left = new Var { symbol = "x" }
    		 right =new Var { symbol = "y" }
    	 }
     }
  }
  /**Mapping variables to constant values and test the expression
   */
  val map = new HashMap[String, Double]
  map += "y" -> 0
  println(term.replace(map).simplify.show)
  map += "x" -> 5
  println(term.replace(map).simplify.eval)
}