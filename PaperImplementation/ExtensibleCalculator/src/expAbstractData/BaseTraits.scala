package expAbstractData

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
