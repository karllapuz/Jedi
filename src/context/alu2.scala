package context

import expression._
import value._

/*
 * Notes:
 * alu implements all low-level arithmetic, logic, and I/O functions
 * alu does lots of type checking
 * alu is a singleton
 */
object alu2 {
  // dispatcher
  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add" => add(args)
      case "mul" => mul(args)
      case "sub" => sub(args)
      case "div" => div(args)
      case "less" => less(args)
      case "more" => more(args)
      case "equals" => equals(args)
      case "unequals" => unequals(args)
      case "not" => not(args)
      // variables
      case "dereference" => dereference(args)
      case "var" => makeVar(args)
      // primitive I/O ops:
      case "write" => write(args)
      case "prompt" => prompt(args)
      case "read" => read(args)
      // store ops
      case "store" => store(args)
      case "put" => put(args)
      case "rem" => rem(args)
      case "contains" => contains(args)
      case "map" => map(args)
      case "filter" => filter(args)
      case "get" => get(args)
      case "addLast" => addLast(args)
      case "size" => size(args)
      case _ => throw new UndefinedException(opcode)
    }
  }
  
  // arithmetic and logic ops
  
  private def toInt(arg: Value): Option[Integer] =
    if (arg.isInstanceOf[Integer]) Some(arg.asInstanceOf[Integer]) else None
    
  private def toReal(arg: Value): Option[Real] =
    if (arg.isInstanceOf[Real]) Some(arg.asInstanceOf[Real]) 
    else if (arg.isInstanceOf[Integer]) Some(Integer.intToReal(arg.asInstanceOf[Integer]))
    else None
    
  private def toText(arg: Value): Option[Text] =
    if (arg.isInstanceOf[Text]) Some(arg.asInstanceOf[Text]) else None
    
  private def add(args: List[Value]) = {
    val args2 = args.map(toInt).filter(_ != None)
    if (args2.size == args.size) args2.flatten.reduce(_+_)
    else {
      val args3 = args.map(toReal).filter(_ != None)
      if (args3.size == args.size) args3.flatten.reduce(_+_)
      else {
        val args4 = args.map(toText).filter(_ != None)
        if (args4.size == args.size) args4.flatten.reduce(_+_)
        else {
          throw new TypeException("Inputs to + must be numbers or texts")
        }
      }
    }
  }
  
  private def mul(args: List[Value]) = {
    val args2 = args.map(toInt).filter(_ != None)
    if (args2.size == args.size) args2.flatten.reduce(_*_)
    else {
      val args3 = args.map(toReal).filter(_ != None)
      if (args3.size == args.size) args3.flatten.reduce(_*_)
      else throw new TypeException("Inputs to * must be numbers")
    }
  }  
  
  private def div(args: List[Value]) = {
    val args2 = args.map(toInt).filter(_ != None)
    if (args2.size == args.size) args2.flatten.reduce(_/_)
    else {
      val args3 = args.map(toReal).filter(_ != None)
      if (args3.size == args.size) args3.flatten.reduce(_/_)
      else throw new TypeException("Inputs to / must be numbers")
    }
  }  
  
  private def sub(args: List[Value]) = {
    val args2 = args.map(toInt).filter(_ != None)
    if (args2.size == args.size) args2.flatten.reduce(_-_)
    else {
      val args3 = args.map(toReal).filter(_ != None)
      if (args3.size == args.size) args3.flatten.reduce(_-_)
      else throw new TypeException("Inputs to - must be numbers")
    }
  }
  
  def less(args: List[Value]): Value = {
    if (args.length  != 2) throw new TypeException("less expects two inputs")
    val args2 = args.map(toInt).filter(_ != None)
    if (args2.size == args.size) Boole(args2(0) < args2(1))
    else {
      val args3 = args.map(toReal).filter(_ != None)
      if (args3.size == args.size) Boole(args3(0) < args3(1))
      else {
        val args4 = args.map(toText).filter(_ != None)
        if (args4.size == args.size) Boole(args4(0).get < args4(1).get)
        else throw new TypeException("Inputs to < must be numbers or texts")
      }
    }
  }  
  
  def more(args: List[Value]): Value = {
    if (args.length  != 2) throw new TypeException("less expects two inputs")
    val args2 = args.map(toInt).filter(_ != None)
    if (args2.size == args.size) Boole(args2(0) > args2(1))
    else {
      val args3 = args.map(toReal).filter(_ != None)
      if (args3.size == args.size) Boole(args3(0) > args3(1))
      else {
        val args4 = args.map(toText).filter(_ != None)
        if (args4.size == args.size) Boole(args4(0).get > args4(1).get)
        else throw new TypeException("Inputs to < must be numbers or texts")
      }
    }
  }
  
  def equals(args: List[Value]): Value = {
     if (args.isEmpty) throw new TypeException("equals expected > 0 inputs")
     var more = true
     var result = true
     for(i <- 1 until args.length if more) {
       {
         if (args(i) != args(0)) {
            result = false
            more = false
         }
       }
     }
     Boole(result)
  }
  
   def unequals(args: List[Value]): Value = {
    if (args.length != 2) throw new TypeException("unequals expected 2 inputs")
    if (args(0) != args(1)) Boole(true) else Boole(false)
  }
  
  
   def not(args: List[Value]): Value = {
     if (args.length != 1) throw new TypeException("not expected 1 input")
     if (!args(0).isInstanceOf[Boole]) throw new TypeException("input to not must be Boole")
     !(args(0).asInstanceOf[Boole]) 
  }

   // IO Ops
   def write(args: List[Value]): Value = { println(args(0)); Notification.DONE }
   def read(args: List[Value]): Value = { val result = io.StdIn.readDouble(); Real(result)}
   def prompt(args: List[Value]): Value = { print("=> "); Notification.DONE }

   // variable ops
   
   // returns the content of args(0)
   private def dereference(args: List[Value]) = {
     if (args.isEmpty) throw new TypeException("content expects > 0 inputs")
     var result = args.head.asInstanceOf[Variable].content
     result
   }
   
   // creates a new variable cobtaining args(0)
   private def makeVar(args: List[Value]) = {
     if (args.isEmpty) throw new TypeException("var expects > 0 input")
     else Variable(args.head)
   }
   
   // store ops
   
   // returns a new store containing args
   private def store(args: List[Value]) = {
    var s = new Store
    args.foreach { s.add(_) }
    s 
   }
   
   // put(v: Value, p: Integer, s: Store) calls s.put(v, p)
   private def put(args: List[Value]) = {
     if (args.size != 3 || !args(1).isInstanceOf[Integer] || !args(2).isInstanceOf[Store]) 
        throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
     args(2).asInstanceOf[Store].put(args(0), args(1).asInstanceOf[Integer])
     Notification.DONE
   } 
   
   // rem(p: Integer, s: Store) calls s.rem(p)
   private def rem(args: List[Value]) = {
    if (args.size != 2 || !args(0).isInstanceOf[Integer] || !args(1).isInstanceOf[Store]) 
        throw new TypeException("expected signature: rem(p: Integer, s: Store)")
     args(1).asInstanceOf[Store].rem(args(0).asInstanceOf[Integer])
     Notification.DONE
   }
   
   // get(p: Integer, s: Store) calls s.get(p)
   private def get(args: List[Value]) = {
    if (args.size != 2 || !args(0).isInstanceOf[Integer] || !args(1).isInstanceOf[Store]) 
        throw new TypeException("expected signature: get(p: Integer, s: Store)")
     args(1).asInstanceOf[Store].get(args(0).asInstanceOf[Integer])
     
   }
  
   // map(f: Closure, s: Store) calls s.map(f)
   private def map(args: List[Value]) = {
      if (args.size != 2 || !args(0).isInstanceOf[Closure] || !args(1).isInstanceOf[Store]) 
        throw new TypeException("expected signature: map(p: Closure, s: Store)")
     args(1).asInstanceOf[Store].map(args(0).asInstanceOf[Closure])
   } 
 
   
   // filter(f: Closure, s: Store) calls s.filter(f)
   private def filter(args: List[Value]) = {
    if (args.size != 2 || !args(0).isInstanceOf[Closure] || !args(1).isInstanceOf[Store]) 
        throw new TypeException("expected signature: filter(p: Closure, s: Store)")
    args(1).asInstanceOf[Store].filter(args(0).asInstanceOf[Closure])
    }   
   // contains(v: Value, s: Store) calls s.contains(v)
   private def contains(args: List[Value]) = {
    if (args.size != 2 || !args(1).isInstanceOf[Store]) 
        throw new TypeException("expected signature: contains(v: Value, s: Store)")
    args(1).asInstanceOf[Store].contains(args.head)
   }
   
   // addLast(v: Value, s: Store) calls s.add(v)
   private def addLast(args: List[Value]) = {
    if (args.size != 2 ||  !args(1).isInstanceOf[Store])
        throw new TypeException("expected signature: add(v: Value, s: Store)")
    args(1).asInstanceOf[Store].add(args.head)
    Notification.DONE
   }
   
   // size(s: Store) calls s.size
   private def size(args: List[Value]) = {
     if (args.size != 1 || !args.head.isInstanceOf[Store])
       throw new TypeException("expected signature: size(s: Store)")
     args.head.asInstanceOf[Store].size
   }
   
  // etc.
}