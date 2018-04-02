package expression
import value._
import context._

trait Literal extends Expression with Value {
  
  def execute(env: Environment): Value = this
}