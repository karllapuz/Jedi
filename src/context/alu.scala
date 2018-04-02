package context


  
import expression._
import value._



/*
 * Notes:
 * alu implements all low-level arithmetic, logic, and I/O functions
 * alu does lots of type checking
 * alu is a singleton
 */
object alu {
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
      // primitive I/O ops:
      case "write" => write(args)
      case "prompt" => prompt(args)
      case "read" => read(args)
      case _ => throw new UndefinedException(opcode)
    }
  }
  
  private def castAsReal(value: Value, opcode: String): Real = {
    value match {
      case n: Integer => Integer.intToReal(n)
      case n: Real => n
      case _ => throw new TypeException(opcode + " inputs must be numbers")
    }
  }
  
  private def castAsText(value: Value, opcode: String): Text = {
    value match {
      case n: Text => n
      case _ => throw new TypeException(opcode + " inputs must be texts")
    }
  }
  
  private def castAsIntegers(vals: List[Value], opcode: String): List[Integer] = {
    if (vals.isEmpty) throw new TypeException(opcode + " expected > 0 inputs")
    val ok = vals.filter(_.isInstanceOf[Integer])
    if (ok.length < vals.length) throw new TypeException(opcode + " inputs must be numbers")
    vals.map(_.asInstanceOf[Integer])
  }  
  
  private def castAsReals(vals: List[Value], opcode: String): List[Real] = {
    if (vals.isEmpty) throw new TypeException(opcode + " expected > 0 inputs")
    vals.map(castAsReal(_, opcode))
  }
  
  private def castAsTexts(vals: List[Value], opcode: String): List[Text] = {
    if (vals.isEmpty) throw new TypeException(opcode + " expected > 0 inputs")
    vals.map(castAsText(_, opcode))
  }
  
  private def castAsBoole(value: Value, opcode: String): Boole = {
    value match {
      case n: Boole => n
      case _ => throw new TypeException(opcode + " inputs must be boole")
    }
  }
  
  private def add(vals: List[Value]): Value = {
    try {
      castAsIntegers(vals, "add").reduce(_+_) 
    } catch {
       case e: TypeException =>
         try {
           castAsReals(vals, "add").reduce(_+_) 
        } catch {
          case e: TypeException => castAsTexts(vals, "concat").reduce(_+_)
      }
    }
  }
  
  private def sub(vals: List[Value]): Value = {
    try {
      castAsIntegers(vals, "sub").reduce(_-_) 
    } catch {
       case e: TypeException =>
         try {
           castAsReals(vals, "sub").reduce(_-_) 
        } catch {
          case e: TypeException => throw new TypeException("can't subtract texts")
      }
    }
  }
  
   private def mul(vals: List[Value]): Value = {
    try {
      castAsIntegers(vals, "mul").reduce(_*_) 
    } catch {
       case e: TypeException =>
         try {
           castAsReals(vals, "mul").reduce(_*_) 
        } catch {
          case e: TypeException => throw new TypeException("can't multiply texts")
      }
    }
  }
   
   private def div(vals: List[Value]): Value = {
    try {
      castAsIntegers(vals, "div").reduce(_/_) 
    } catch {
       case e: TypeException =>
         try {
           castAsReals(vals, "div").reduce(_/_) 
        } catch {
          case e: TypeException => throw new TypeException("can't divide texts")
      }
    }
  }
   
  private def less(vals: List[Value]): Value = {
    if (vals.length  != 2) throw new TypeException("less expects two inputs")
    try {
      val nums = castAsIntegers(vals, "less")
      Boole(nums(0) < nums(1))
    } catch {
      case e: TypeException => {
        try {
          val nums = castAsReals(vals, "less")
          Boole(nums(0) < nums(1))
        } catch {
          case e: TypeException => {
            val texts = castAsTexts(vals, "less")
            Boole(texts(0) < texts(1))
          }
        }
      }
    }
  }
  
  private def more(vals: List[Value]): Value = {
    if (vals.length  != 2) throw new TypeException("more expects two inputs")
    try {
      val nums = castAsIntegers(vals, "more")
      Boole(nums(0) > nums(1))
    } catch {
      case e: TypeException => {
        try {
          val nums = castAsReals(vals, "more")
          Boole(nums(0) > nums(1))
        } catch {
          case e: TypeException => {
            val texts = castAsTexts(vals, "more")
            Boole(texts(0) > texts(1))
          }
        }
      }
    }
  }
  
  private def equals(vals: List[Value]): Value = {
    if (vals.length  < 2) throw new TypeException("equals expects at least two inputs")
    try {
      val nums = castAsIntegers(vals, "equals")
      val result = nums.map((x:Integer) => x == nums(0)).reduce(_&&_)
      Boole(result)
    } catch {
      case e: TypeException => {
        try {
          val nums = castAsReals(vals, "equals")
          val result = nums.map((x:Real) => x == nums(0)).reduce(_&&_)
          Boole(result)
        } catch {
          case e: TypeException => {
            val texts = castAsTexts(vals, "equals")
            val result = texts.map((x:Text) => x == texts(0)).reduce(_&&_)
            Boole(result)
          }
        }
      }
    }
  }
  
  private def unequals(vals: List[Value]): Value = {
    if (vals.length  != 2) throw new TypeException("unequals expects two inputs")
    not(List(equals(vals)))
  }
  
  private def not(vals: List[Value]): Value = {
    if (vals.length  != 1) throw new TypeException("not expects only one input")
    try {
      val bool = castAsBoole(vals.head, "not")
      !bool
    } catch {
        case e: TypeException => throw new TypeException("input must be boole")       
        }
  }
 
   def write(vals: List[Value]): Value = { println(vals(0)); Notification.DONE }
   def read(vals: List[Value]): Value = { val result = io.StdIn.readDouble(); Real(result)}
   def prompt(vals: List[Value]): Value = { print("=> "); Notification.DONE }

  
  // etc.
}