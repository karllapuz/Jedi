package expression

import context._
import value._

case class Conjunction(val operands: List[Expression]) extends SpecialForm {
  
  def execute(env: Environment): Value = {
    var isTrue = true
    var i = 0
    while (isTrue && i < operands.length) {
      val result = operands(i).execute(env)
      if (!result.isInstanceOf[Boole]) throw new TypeException("Type mismatch:  " + result)
      isTrue = (result.asInstanceOf[Boole]).value
      i = i + 1
    }
    new Boole(isTrue)
  }
  
}