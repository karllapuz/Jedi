package expression

case class Integer(val value: Int) extends Literal with Ordered[Integer] with Equals {
  
  def +(other: Integer) = Integer(this.value + other.value)
  def -(other: Integer) = Integer(this.value - other.value)
  def *(other: Integer) = Integer(this.value * other.value)
  def /(other: Integer) = {
    other match {
      case Integer(0) => throw new Exception("Can't divide by 0")
      case _ => Integer(this.value / other.value)
    }
  }
  override def toString = value.toString
  def compare(other: Integer): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
  override def canEqual(other: Any) =  other.isInstanceOf[Integer]
  override def equals(other: Any): Boolean = 
    other match {
       case other: Integer => this.canEqual(other) && (other.value == this.value)
       case _ => false
    }
  override def hashCode = this.toString.##
  
}

object Integer {
  
  implicit def intToReal(n: Integer): Real = new Real(n.value.toDouble)
  
}