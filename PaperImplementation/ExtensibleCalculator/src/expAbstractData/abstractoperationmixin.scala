package expAbstractData

import scala.collection.mutable.HashMap
/** A base class consisting of
 *   - a root trait (i.e. abstract class) `Exp' with an `eval' function
 *   - an abstract type `exp' bounded by `Exp'
 *   - a concrete instance class `Num' of `Exp' for numeric literals
 */
trait Base {
  type exp <: Exp

  trait Exp { 
    def eval: Double
  }

  trait Num extends Exp {
    var value: Double = _ ;
    def eval = value
  }
  
  trait Var extends Exp {
    var symbol: String = _;
    def eval:Double = throw new Exception("Variable not defined.");
    def sEval = symbol
  }
}


/** Data extension 1: An extension of `Base' with `Plus' expressions
 */
trait BasePlus extends Base {
  trait Plus extends Exp {
    var left: exp = _ ;
    var right: exp = _ ;
    def eval = left.eval + right.eval
  }
}

/** Data extension 2: An extension of `Base' with `Subt' expressions
 */
trait BaseSubt extends Base {
  trait Subt extends Exp {
    var left: exp = _ ;
    var right: exp = _ ;
    def eval = left.eval - right.eval
  }
} 

/** Data extension 3: An extension of `Base' with negation expressions
 */
trait BaseNeg extends Base {
  trait Neg extends Exp {
    var operand: exp = _ ;
    def eval = - operand.eval
  }
}

/** Data extension 4: An extension of `Base' with `Mult' expressions
 */
trait BaseMult extends Base {
  trait Mult extends Exp {
    var left: exp = _ ;
    var right: exp = _ ;
    def eval = left.eval * right.eval
  }
}

/** Data extension 5: An extension of `Base' with `Div' expressions
 */
trait BaseDiv extends Base {
  trait Div extends Exp {
    var left: exp = _ ;
    var right: exp = _ ;
    def eval = left.eval / right.eval
  }
} 

/** Data extension 6: An extension of `Base' with `Bracket' expressions
 */
trait BaseBracket extends Base {
  trait Bracket extends Exp {
    var operand: exp = _ ;
    def eval = operand.eval
  }
} 

/** Combining all data extensions
 */
trait BaseAllOperations extends BasePlus with BaseNeg with BaseSubt with BaseMult with BaseDiv with BaseBracket{
  def Num(v: Double): exp
  def Var(s: String): exp
  def Plus(l: exp, r: exp): exp
  def Subt(l: exp, r: exp): exp
  def Mult(l: exp, r: exp): exp
  def Div(l: exp, r: exp): exp
  def Bracket(o: exp): exp
  def Neg(t: exp): exp
}

/** A test object for the combination class.
 *  It ``ties the knot'' by equating the abstract type `exp' with
 *  the root class `Exp'.
 */

///////////////////////////////////////////////////////////////////

/** Operation extension 1: An extension of `Base' with a `show' function.
 *  This class needs
 *    - a new root class `Exp' which adds a `show' method to `super.Exp',
 *      the old root class in `Base', 
 *    - a rebinding of the abstract type `exp' to be a subtype of the new root class,
 *    - a new class for numeric literals which adds a `show' method to `super.Num'.
 */
trait Show extends Base {
  type exp <: Exp

  trait Exp extends super.Exp {
    def show: String
  }
  trait Num extends super.Num with Exp {
    def show = value.toString()
  }
  trait Var extends super.Var with Exp{
    def show = sEval
  }  
}

/** Combining operation extension 1 with the data extensions:
 *    - we only need to implement `show' for the two extension classes 
 *      `Plus` and `Neg`.
 */
trait ShowAllOperations extends BaseAllOperations with Show{
  trait Plus extends super.Plus with Exp {
    def show = left.show + "+" + right.show
  }
  trait Neg extends super.Neg with Exp {
    def show = "-(" + operand.show + ")"
  }
  trait Subt extends super.Subt with Exp {
    def show = left.show + "-" + right.show
  }
  trait Mult extends super.Mult with Exp {
    def show = left.show + "*" + right.show
  }
  trait Div extends super.Div with Exp {
    def show = left.show + "/" + right.show
  }
  trait Bracket extends super.Bracket with Exp {
    def show = "(" + operand.show + ")"
  }
}


trait SimplifyAllOperations extends BaseAllOperations{
  type exp <: Exp
  
  trait Exp extends super.Exp {
    def simplify: exp
  }
  
  trait Num extends super.Num with Exp {
    def simplify = Num (value) 
  }
  trait Var extends super.Var with Exp {
    def simplify = Var (symbol) 
  }
  trait Neg extends super.Neg with Exp{
    def simplify = Neg(operand.simplify)
  }
  trait Bracket extends super.Bracket with Exp{
    def simplify = Bracket(operand.simplify)
  }
  
  trait Plus extends super.Plus with Exp{
    def simplify:exp = {
      var sLeft = left.simplify
      var sRight = right.simplify
      // x + 0
      if ((sLeft.isInstanceOf[Var] && (sRight.isInstanceOf[Num] && (sRight.eval == 0))) )
      {
        var casted = sLeft.asInstanceOf[Var]
        return Var(casted.symbol)
      }
      // 0 + x
      else if((sRight.isInstanceOf[Var] && (sLeft.isInstanceOf[Num] && (sLeft.eval == 0))) )
      {
        var casted = sRight.asInstanceOf[Var]
        return Var(casted.symbol)
      }
      // x + x
      else if (sLeft.isInstanceOf[Var] && sRight.isInstanceOf[Var] ) 
      {
        var castedLeft = sLeft.asInstanceOf[Var]
        var castedRight = sRight.asInstanceOf[Var]
        if(castedLeft.symbol == castedRight.symbol)
        {
          return Mult(Num(2),Var(castedLeft.symbol))
        }
      }
      // (e1*x) + (e2*x) || (x*e1)+(e2*x) || (e1*x)+(x*e2) || (x*e1)+(x*e2)
      else if (sLeft.isInstanceOf[Mult] && sRight.isInstanceOf[Mult] )
      {
        var firstMult = sLeft.asInstanceOf[Mult]
        var secondMult = sRight.asInstanceOf[Mult]
        
        var first = firstMult.left;
        var second = firstMult.right;
        var third = secondMult.left;
        var fourth = secondMult.right;
        
        //(e1*x) + (e2*x)
        if(second.isInstanceOf[Var] && fourth.isInstanceOf[Var])
        {
          if(second.asInstanceOf[Var].symbol == fourth.asInstanceOf[Var])
          {
            return Mult(Plus(first,third),fourth )
          }
        }
        //(x*e1)+(e2*x
        else if(first.isInstanceOf[Var] && fourth.isInstanceOf[Var])
        {
          if(first.asInstanceOf[Var].symbol == fourth.asInstanceOf[Var])
          {
            return Mult(Plus(second,third),fourth )
          }
        }
        //(e1*x)+(x*e2)
        else if(second.isInstanceOf[Var] && third.isInstanceOf[Var])
        {
          if(second.asInstanceOf[Var].symbol == third.asInstanceOf[Var])
          {
            return Mult(Plus(first,fourth),third )
          }
        }
        //(x*e1)+(x*e2)
        else if(first.isInstanceOf[Var] && third.isInstanceOf[Var])
        {
          if(first.asInstanceOf[Var].symbol == third.asInstanceOf[Var])
          {
            return Mult(Plus(second,fourth),third )
          }
        }
      }
      return Plus(sLeft,sRight)
    }
  }
  trait Subt extends super.Subt with Exp{
    def simplify:exp = {
      var sLeft = left.simplify
      var sRight = right.simplify
      
      //checking for x - x
      if (sLeft.isInstanceOf[Var] && sRight.isInstanceOf[Var] )
      {
        if (sLeft.asInstanceOf[Var].symbol == sRight.asInstanceOf[Var].symbol)
        {
            return Num(0)
        }
      }      
      return Subt(sLeft,sRight)
    }
  }
  trait Mult extends super.Mult with Exp{
    def simplify:exp = {
      var sLeft = left.simplify
      var sRight = right.simplify
      
      //checking for x * 1
      if (sLeft.isInstanceOf[Var] && sRight.isInstanceOf[Num] )
      {
        if ( sRight.asInstanceOf[Num].eval == 1)
        {
            return Var(sLeft.asInstanceOf[Var].symbol)
        }
      }
      //checking for 1 * x
      else if (sRight.isInstanceOf[Var] && sLeft.isInstanceOf[Num] )
      {
        if (sLeft.asInstanceOf[Num].eval == 1)
        {
          return Var(sRight.asInstanceOf[Var].symbol)
        }
      }
      return Mult(sLeft,sRight)
    }
  }
  trait Div extends super.Div with Exp{
    def simplify:exp = {
      var sLeft = left.simplify
      var sRight = right.simplify
      
      //checking for x / 1
      if (sLeft.isInstanceOf[Var] && sRight.isInstanceOf[Num] )
      {
        if ( sRight.asInstanceOf[Num].eval == 1)
        {
            return Var(sLeft.asInstanceOf[Var].symbol)
        }
      }
      //checking for (e*x)/x || (x*e)
      else if (sLeft.isInstanceOf[Mult] && sRight.isInstanceOf[Var] )
      {
        var mult = sLeft.asInstanceOf[Mult];
        if(mult.left.isInstanceOf[Var])
        {
          if(mult.left.asInstanceOf[Var].symbol == sRight.asInstanceOf[Var].symbol)
          {
            return mult.right
          }
        }
        else if(mult.right.isInstanceOf[Var])
        {
          if(mult.right.asInstanceOf[Var].symbol == sRight.asInstanceOf[Var].symbol)
          {
            return mult.left
          }
        }
      }
      return Div(sLeft,sRight)
    }
  }


}

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


