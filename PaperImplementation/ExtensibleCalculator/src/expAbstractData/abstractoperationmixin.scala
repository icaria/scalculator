package expAbstractData

/** A base class consisting of
 *   - a root trait (i.e. abstract class) `Exp' with an `eval' function
 *   - an abstract type `exp' bounded by `Exp'
 *   - a concrete instance class `Num' of `Exp' for numeric literals
 */
trait Base {
  type exp <: Exp

  trait Exp { 
    def eval: Int
    def name: String
  }

  trait Num extends Exp {
    val value: Int
    def name = "Num"
    def eval = value
  }
  
  trait Var extends Exp {
    val value: Int
    val symbol: String
    def name = "Var"
    def eval = value
    def sEval = symbol
  }
}



/** Data extension 1: An extension of `Base' with `Plus' expressions
 */
trait BasePlus extends Base {
  trait Plus extends Exp {
    val left: exp
    val right: exp
    def eval = left.eval + right.eval
    def name = "Plus"
  }
}

/** Data extension 2: An extension of `Base' with `Subt' expressions
 */
trait BaseSubt extends Base {
  trait Subt extends Exp {
    val left: exp
    val right: exp
    def eval = left.eval - right.eval
    def name = "Subt"
  }
} 

/** Data extension 3: An extension of `Base' with negation expressions
 */
trait BaseNeg extends Base {
  trait Neg extends Exp {
    val operand: exp
    def name = "Neg"
    def eval = - operand.eval
  }
}

/** Data extension 4: An extension of `Base' with `Mult' expressions
 */
trait BaseMult extends Base {
  trait Mult extends Exp {
    val left: exp
    val right: exp
    def eval = left.eval * right.eval
    def name = "Mult"
  }
}

/** Data extension 5: An extension of `Base' with `Div' expressions
 */
trait BaseDiv extends Base {
  trait Div extends Exp {
    val left: exp
    val right: exp
    def eval = left.eval / right.eval
    def name = "Div"
  }
} 

/** Data extension 6: An extension of `Base' with `Bracket' expressions
 */
trait BaseBracket extends Base {
  trait Bracket extends Exp {
    val operand: exp
    def eval = eval
    def name = "Bracket"
  }
} 

/** Combining the plus and negation data extensions
 */
trait BasePlusNeg extends BasePlus with BaseNeg

/** Combining all data extensions
 */
trait BaseAllOperations extends BasePlusNeg with BaseSubt with BaseMult with BaseDiv with BaseBracket

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
}

/** Combining operation extension 1 with the data extensions:
 *    - we only need to implement `show' for the two extension classes 
 *      `Plus` and `Neg`.
 */
trait ShowPlusNeg extends BasePlusNeg with Show {
  trait Plus extends super.Plus with Exp {
    def show = left.show + "+" + right.show
  }
  trait Neg extends super.Neg with Exp {
    def show = "-(" + operand.show + ")"
  }
}


/*
object error {
  val t1 = new testShowPlusNeg.Num { val value = 1 }
  val t2 = new testDblePlusNeg.Neg { val value = t1 }
  val t3 = t1.dble
}
*/  

////////////////////////////////////////////////////////////////////////

/** Operation extension 2: An extension of `BasePlusNeg' with a `dble' method,
 *  which returns an expression tree representing the original tree times two.
 *  - The extension class redefines all data classes of `BasePlusNeg', adding
 *    a `dble' method to each.
 *  - Note that we need factory methods to build expression trees of the right
 *    type (either DblePlusNeg.Exp, or an extension thereof).
 */
trait DblePlusNeg extends BasePlusNeg {
  type exp <: Exp

  trait Exp extends super.Exp {
    def dble: exp
  }

  def Num(v: Int): exp
  def Plus(l: exp, r: exp): exp
  def Neg(t: exp): exp

  trait Num extends super.Num with Exp {
    def dble = Num(value * 2)
  }
  trait Plus extends super.Plus with Exp {
    def dble = Plus(left.dble, right.dble)
  }
  trait Neg extends super.Neg with Exp {
    def dble = Neg(operand.dble)
  }
}

/** Testing the resulting combination
 */


/** Combining both operation extensions:
 *  - This is done by a composing corresponding data classes of each extension.
 */
trait ShowDblePlusNeg extends ShowPlusNeg with DblePlusNeg {
  type exp <: Exp

  trait Exp extends super[ShowPlusNeg].Exp with super[DblePlusNeg].Exp

  trait Num 
    extends super[ShowPlusNeg].Num 
       with super[DblePlusNeg].Num 
       with Exp

  trait Plus 
    extends super[ShowPlusNeg].Plus
       with super[DblePlusNeg].Plus
       with Exp

  trait Neg
    extends super[ShowPlusNeg].Neg 
       with super[DblePlusNeg].Neg
       with Exp
}

/** Testing the resulting combination
 */


/////////////////////////////////////////////////////////////

/** Binary methods
*/
trait Equals extends Base {
  type exp <: Exp
  trait Exp extends super.Exp {
    def eql(other: exp): Boolean
    def isNum(v: Int) = false
  }
  trait Num extends super.Num with Exp {
    def eql(other: exp): Boolean = other.isNum(value)
    override def isNum(v: Int) = v == value
  }
}

trait EqualsPlusNeg extends BasePlusNeg with Equals {
  type exp <: Exp
  trait Exp extends super[BasePlusNeg].Exp with super[Equals].Exp {
    def isPlus(l: exp, r: exp): Boolean = false
    def isNeg(t: exp): Boolean = false
  }
  trait Num extends super[Equals].Num with Exp
  trait Plus extends Exp with super.Plus {
    def eql(other: exp): Boolean = other.isPlus(left, right)
    override def isPlus(l: exp, r: exp) = (left eql l) && (right eql r)
  }
  trait Neg extends Exp with super.Neg {
    def eql(other: exp): Boolean = other.isNeg(operand)
    override def isNeg(t: exp) = operand eql t
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

object testEqualsAllOperations extends EqualsAllOperations with Application
{
  type exp = Exp
  val term = new Mult{
    val left = new Plus { 
      val left = new Num { val value =1 }
      val right = new Num { val value = 2}
      }
    val right = new Num {
      val value = 2
      }
  }
  val divTerm = new Div{
    val left = term
    val right = new Num {val value = 3}
  }
  
  val subtTerm = new Subt{
    val left = divTerm
    val right = new Num {val value = 2}
  }
  System.out.println("yoyoyo \n")
  System.out.println(term eval)
  System.out.println("divTerm ",divTerm eval)
  System.out.println(subtTerm eval)
  
  //System.out.println(term eql new Num { val value = 1 })
   //def main(args: Array[String]) {println("yoyoyo \n")}
  
}



trait EqualsShowPlusNeg extends EqualsPlusNeg with ShowPlusNeg {
  type exp <: Exp

  trait Exp extends super[EqualsPlusNeg].Exp with super[ShowPlusNeg].Exp

  trait Num
    extends super[EqualsPlusNeg].Num
       with super[ShowPlusNeg].Num
       with Exp

  trait Plus 
    extends super[EqualsPlusNeg].Plus
       with super[ShowPlusNeg].Plus
       with Exp

  trait Neg 
    extends super[EqualsPlusNeg].Neg
       with super[ShowPlusNeg].Neg
       with Exp
}