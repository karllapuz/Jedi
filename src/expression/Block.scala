/*package expression

import context._
import value._

case class Block(val exps: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    // Create a temporary environment
    var tempEnv = new Environment(env)
    // Execute every line in list of expressions
    for (i <- 0 until exps.length - 1) exps(i).execute(tempEnv)
    // Return the last executed expression
    exps(exps.length - 1).execute(tempEnv)
  }
  
}*/

package expression

import context._
import value._

case class Block(val exps: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    // Create a temporary environment
    var result: Value = Notification("unspecified")
    var localEnv = new Environment(env)
    // Execute every line in list of expressions
    for (exp <- exps) result = exp.execute(localEnv)
    // Return the last executed expression
    result
  }
  
}