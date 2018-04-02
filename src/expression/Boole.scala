package expression

case class Boole(val value: Boolean) extends Literal {
  
  def &&(other: Boole) = if (this.value && other.value) Boole(true) else Boole(false)
  def ||(other: Boole) = if (this.value || other.value) Boole(true) else Boole(false)
  def unary_! = new Boole(!this.value)
  
  override def toString() = value.toString()
  
}