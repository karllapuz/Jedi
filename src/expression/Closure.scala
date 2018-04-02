package expression

import context._
import value._

case class Closure(val params: List[Identifier], val body: Expression, val env: Environment) extends Value {
  
  def apply(args: List[Value], anEnv: Environment = null): Value = {
    var tempEnv = new Environment()
    if (anEnv == null) tempEnv = new Environment(env)
    else tempEnv = anEnv
    
    if (args.length != params.length) throw new TypeException
    else {
      tempEnv.bulkPut(params, args)
      body.execute(tempEnv)
    }
  }
  
  override def toString = "lambda" + " " + params + " " + body
}