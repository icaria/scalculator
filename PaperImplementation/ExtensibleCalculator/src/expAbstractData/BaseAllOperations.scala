package expAbstractData

/** This trait will setup all required data traits which inherit from the Base class
 * By combining all operations in to one trait, it will be easy to extend all of them 
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
