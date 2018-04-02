package expression

case class Text(val value: String) extends Literal with Ordered[Text] with Equals {
  
  override def <(other: Text): Boolean = this.value < other.value
  override def >(other: Text): Boolean = this.value > other.value
  def ==(other: Text): Boolean = this.value == other.value
  def substring(min: Integer, max: Integer) = Text(this.value.substring(min.value, max.value))
  def +(other: Text) = Text(this.value + other.value)
 
  override def toString() = value.toString()
  def compare(other: Text): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
  override def canEqual(other: Any) =  other.isInstanceOf[Text]
  override def equals(other: Any): Boolean = 
    other match {
       case other: Text => this.canEqual(other) && (other.value == this.value)
       case _ => false
    }
  override def hashCode = this.toString.##
  
}