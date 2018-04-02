package expression

import context._
import value._

case class Lambda(var params: List[Identifier], var body: Expression) extends SpecialForm{
 
  def execute(env: Environment): Value = {
    new Closure(params, body, env)
  }
}