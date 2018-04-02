/*package expression

import context._
import value._

case class Assignment(var vbl: Identifier, var update: Expression) extends SpecialForm {
   
  def execute(env: Environment): Value = {
    var foo = vbl.execute(env)
    try {
      foo.isInstanceOf[Variable]
      foo.asInstanceOf[Variable].content = update.execute(env)
      Notification.DONE      
    }
    catch {
      case e: TypeException => throw new TypeException("Assignment must be a variable")
    }
  }
}*/

package expression
import context.Environment 
import context.TypeException
import value._

case class Assignment (val idf : Identifier, val update: Expression) extends SpecialForm {
  def execute(env: Environment) :Value = {
    var result = idf.execute(env)
    if(!result.isInstanceOf[Variable]) throw new TypeException("Assignment has to be a Variable!")
    else {
      result.asInstanceOf[Variable].content = update.execute(env)
      Notification.DONE
    }
    
  }
}