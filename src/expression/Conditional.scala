package expression

import context._
import value._

case class Conditional(val condition: Expression, val consequent: Expression, val alt: Expression = null) extends SpecialForm {
    def execute(env: Environment): Value = {
      if (condition.execute(env).toString() == "true") consequent.execute(env)
      else if (alt != null) alt.execute(env)
      else Notification.UNSPECIFIED
    }
}