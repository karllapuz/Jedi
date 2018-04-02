package test

import value._
import expression._

object TextTest extends App {
  
  val s1 = Text("California")
  val s2 = Text("...")
  val s3 = Text("Dreaming")
  val s4 = s1 + s2 + s3
  
  println("s4 = " + s4)
  
  println(s4.substring(Integer(3), Integer(8)))
  
  println(s1 + " < " + s3 + " = " + (s1 < s3))
  
}