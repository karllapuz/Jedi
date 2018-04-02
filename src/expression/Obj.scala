package expression

import value._
import context._

case class Obj(val members: List[Declaration], val delegate: Identifier = null) extends SpecialForm {
  
  def execute(env: Environment): Value = {
    // Create tempEnv extending env
    var tempEnv = new Environment(env)
    // Delegate if delegate is not null
    if (delegate != null) {
      var delegation = tempEnv(delegate)
      tempEnv = delegation.asInstanceOf[Environment]
    }
    // Execute members in tempEnv
    for (member <- members) member.execute(tempEnv)
    // Return tempEnv
    tempEnv
  }
}