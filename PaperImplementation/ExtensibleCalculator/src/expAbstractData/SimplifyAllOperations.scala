package expAbstractData

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
