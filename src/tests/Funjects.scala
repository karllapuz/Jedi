package test

object Funjects extends App {
  
  def makeAccount(initBalance: Double) = {
    var balance = initBalance
    def deposit(amt: Double) { balance += amt }
    def withdraw(amt: Double) { balance -= amt }
    def getBalance() = balance
    def dispatcher(cmmd: String, amt: Double = 0.0): Option[Double] = {
      var result: Option[Double] = None;
      cmmd match {
        case "deposit" => deposit(amt)
        case "withdraw" => withdraw(amt)
        case "getBalance" => result = Some(getBalance())
        case _ => throw new Exception("Unrecognized method: " + cmmd)
      }
      result
    }
    dispatcher _   
  }
  
  val savings = makeAccount(54.12)
  val checking = makeAccount(100.0)
  
  savings("deposit", 30)
  checking("withdraw", 30)
  println("savings = " + savings("getBalance", 0)) // prints savings = Some(84.12). 0 needed for some reason
}