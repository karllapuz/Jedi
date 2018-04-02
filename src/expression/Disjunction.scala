package expression

import context._
import value._

case class Disjunction(exp: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    var isTrue = false
    var i = 0
    while (!isTrue && i < exp.length) {
      var result = exp(i).execute(env)
      if (!result.isInstanceOf[Boole]) throw new TypeException("Type mismatch : " + result)
      isTrue = (result.asInstanceOf[Boole]).value
      i = i + 1
    }
    new Boole(isTrue)
  }
  
}