package expAbstractData
import scala.collection.mutable.HashMap




/** A test object for the combination class.
 *  It ``ties the knot'' by equating the abstract type `exp' with
 *  the root class `Exp'.
 */





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
  //println(term.left.show)
  //println(term.right.show)
  val map = new HashMap[String, Double]
  map += "y" -> 0
  println(term.replace(map).simplify.show)
  map += "x" -> 5
  println(term.replace(map).simplify.eval)
}

/*////////////////////////////////////////////////////////////

/** Binary methods
*/
trait Equals extends Base {
  type exp <: Exp
  trait Exp extends super.Exp {
    def eql(other: exp): Boolean
    def isNum(v: Double) = false
  }
  trait Num extends super.Num with Exp {
    def eql(other: exp): Boolean = other.isNum(value)
    override def isNum(v: Double) = v == value
  }
}


trait EqualsAllOperations extends BaseAllOperations with Equals{
  type exp <: Exp
  trait Exp extends super[BaseAllOperations].Exp with super[Equals].Exp{
    def isPlus(l: exp, r: exp): Boolean = false
    def isSubt(l: exp, r: exp): Boolean = false
    def isMult(l: exp, r: exp): Boolean = false
    def isDiv(l: exp, r: exp): Boolean = false
    def isVar(v: String) = false
    def isNeg(t: exp): Boolean = false
  }
  
  trait Num extends super[Equals].Num with Exp
  
  trait Plus extends Exp with super.Plus{
    def eql(other:exp): Boolean = other.isPlus(left, right)
    override def isPlus(l: exp, r: exp) = (left eql l) && (right eql r)
  }
  
  trait Subt extends Exp with super.Subt{
    def eql(other:exp): Boolean = other.isSubt(left, right)
    override def isSubt(l: exp, r: exp) = (left eql l) && (right eql r)
  }
  
  trait Mult extends Exp with super.Mult{
    def eql(other:exp): Boolean = other.isMult(left, right)
    override def isMult(l: exp, r: exp) = (left eql l) && (right eql r)
  }
  
  trait Div extends Exp with super.Div{
    def eql(other:exp): Boolean = other.isDiv(left, right)
    override def isDiv(l: exp, r: exp) = (left eql l) && (right eql r)
  }
  trait Var extends Exp with super.Var {
    def eql(other: exp): Boolean = other.isVar(symbol)
    override def isVar(v: String) = v == symbol
  }
  trait Neg extends Exp with super.Neg {
    def eql(other: exp): Boolean = other.isNeg(operand)
    override def isNeg(t: exp) = operand eql t
  }
}

trait SimplifyEqualsAllOperations extends SimplifyAllOperations with EqualsAllOperations
{
  type exp <: Exp
  trait Exp extends super[SimplifyAllOperations].Exp with super[EqualsAllOperations].Exp
  trait Num extends super[SimplifyAllOperations].Num with super[EqualsAllOperations].Num
  trait Bracket extends super[SimplifyAllOperations].Bracket with super[EqualsAllOperations].Bracket
  trait Neg extends super[SimplifyAllOperations].Neg with super[EqualsAllOperations].Neg
  trait Plus extends super[SimplifyAllOperations].Plus with super[EqualsAllOperations].Plus
  trait Subt extends super[SimplifyAllOperations].Subt with super[EqualsAllOperations].Subt
  trait Mult extends super[SimplifyAllOperations].Mult with super[EqualsAllOperations].Mult
  trait Div extends super[SimplifyAllOperations].Div with super[EqualsAllOperations].Div
}

object testEqualsAllOperations extends EqualsAllOperations with Application
{
  type exp = Exp
  var term = new Mult{
     left =new Plus { 
      left = new Num { value = 1 }
       right = new Num {  value = 2}
      }
     right =new Num { value =2   }
  }
  var divTerm = new Div{
     left = term
     right = new Num {value =3}
  }
  
  var subtTerm = new Subt{
    left = divTerm
    right = new Num { value = 2}
  }
  
  def replace(e:exp): exp =
  {
    //if  (e.name =="Mult") {
      System.out.println("troll")
    //} 
	   new Plus { 
	      left = new Num{  value = 0}
	      right = new Num { value = 0}
	  }
  }
  var mult:exp = new Mult { 
    left = new Num{ value = 0}
    right = new Num { value = 0}
  }
  
  //Important
  //def Num(v : Int) = new Num{ var value = v}
  
  mult = replace(mult)
  
  //System.out.println(mult.name)
  System.out.println("yoyoyo \n")
  System.out.println(term eval)
  System.out.println("divTerm ",divTerm eval)
  System.out.println(subtTerm eval)
  
}*/


