package expression

import context._
import value._

case class Iteration(var condition: Expression, var body: Expression) extends SpecialForm {
  
  def execute(env: Environment): Value = {
    var foo = condition.execute(env)
    var result: Value = Notification.UNSPECIFIED
    if (foo.isInstanceOf[Boole]) {
      
      while (condition.execute(env).toString()=="true") {
        result = body.execute(env)
      }
      result
    } else
      throw new TypeException("Type Exception error")
  }
}