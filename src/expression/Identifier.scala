package expression

import context._
import value._

case class Identifier(val name: String, val names: List[String] = Nil) extends Expression{
  
  override def toString = name
  def execute(env: Environment): Value = {
    
    // Jedi 4.0
    // x = env(this)
    // if (names != Nil && x.isInstanceOf[Environment])
    //   currentEnv = x.asInstanceof[Environment])
    //   for (name <- names) {
    //     currentEnv = currentEnv(Identifier(name))
    //   }
    var x = env(Identifier(name))
    
    if (names != Nil) { 
      var currentEnv = env
      if (x.isInstanceOf[Environment]) currentEnv = x.asInstanceOf[Environment]
      else throw new TypeException(x + "undefined")
      
      for (i <- 0 until names.length - 1) {
        x = currentEnv(Identifier(names(i)))
        if (x.isInstanceOf[Environment]) {
          currentEnv = x.asInstanceOf[Environment]
        }
        else throw new TypeException(x + "undefined")
      }
        x = currentEnv(Identifier(names.last)) 
    }
    x
  }
}