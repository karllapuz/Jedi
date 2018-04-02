package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi2Parsers extends Jedi1Parsers {
  
  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def params: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep(","~>identifier))<~")" ^^ {
    case None => Nil
    case Some(e ~ Nil) => List(e)
    case Some(e ~ exp) => e::exp
    case _ => Nil
  }
  
  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda: Parser[Expression] = "lambda" ~ params ~ expression ^^ {
    case "lambda" ~ params ~ exp => Lambda(params, exp)
  }
  
  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def block: Parser[Expression] = "{" ~ expression ~ rep( ";" ~> expression) ~ "}" ^^ {
    case "{" ~ exp ~ Nil ~ "}" => Block(List(exp))
    case "{" ~ exp ~ more ~ "}" => Block(exp::more)
  }
  
  
  // override of term parser
  override def term: Parser[Expression]  = lambda | funCall | block | literal | "("~>expression<~")"
}