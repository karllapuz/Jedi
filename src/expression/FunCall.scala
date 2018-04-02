package expression

import context._
import value._

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {
  
  def execute(env: Environment): Value = {
    /* JEDI 1.0 
    // eagerly convert operands to arguments (values)
    val operandValues = operands.map(_.execute(env))
    // ask alu to execute
    alu.execute(operator, operandValues)
    */
    
    // choice of dynamic or static scoping
    val staticScope = true
    var anEnv = env;
    if (staticScope) anEnv = null
    
    // eagerly convert operands to arguments (values)
    val operandValues = operands.map(_.execute(env))
    try {
      val func = operator.execute(env)
      if (func.isInstanceOf[Closure]) func.asInstanceOf[Closure].apply(operandValues, anEnv)
      else throw new TypeException(operator + " undefined")
    }
    catch 
    {
      case e: UndefinedException => alu2.execute(operator.asInstanceOf[Identifier], operandValues) // ask alu to execute
    }
  }
}