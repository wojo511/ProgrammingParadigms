object lab4 extends App {

  val primes = (toN: Int) => {

    def findPrimes(sieve: List[Int]): List[Int] = sieve match {

      case h :: t => h :: findPrimes(t filter (x => x % h != 0))
      case Nil => Nil

    }

    findPrimes(List.range(2, toN))
  }

  def returnPrimesFromList(list: List[Int]): List[Int] = {

    val listOfPrimes = primes(200)

    list.intersect(listOfPrimes)

  }

  sealed trait Calculator

  case class Addition(x: Int, y: Int) extends Calculator

  case class Negation(x: Int) extends Calculator

  def calculator1(x: Calculator): Int = x match {

    case Addition(x, y) => x + y
    case Negation(x) => -x
  }


  enum Calculator2:
    case Add(x: Int, y: Int)
    case Neg(x: Int)


  def calculator2(x: Calculator2): Int = x match {

    case Calculator2.Add(x, y) => x + y
    case Calculator2.Neg(x) => -x
  }

  sealed trait Bool

  case class AND(x: Boolean, y: Boolean) extends Bool

  case class OR(x: Boolean, y: Boolean) extends Bool

  case class XOR(x: Boolean, y: Boolean) extends Bool

  case class NAND(x: Boolean, y: Boolean) extends Bool

  case class NOR(x: Boolean, y: Boolean) extends Bool


  def and(x: Bool): Boolean = x match {

    case AND(true, true) => true
    case AND(true, false) => false
    case AND(false, true) => false
    case AND(false, false) => false

  }

  def or(x: Bool): Boolean = x match {

    case OR(true, true) => true
    case OR(true, false) => true
    case OR(false, true) => true
    case OR(false, false) => false

  }

  def xor(x: Bool): Boolean = x match {

    case XOR(true, true) => false
    case XOR(true, false) => true
    case XOR(false, true) => true
    case XOR(false, false) => false
  }

  def nand(x: Bool): Boolean = x match {

    case NAND(true, true) => false
    case NAND(true, false) => true
    case NAND(false, true) => true
    case NAND(false, false) => true

  }

  def nor(x: Bool): Boolean = x match {

    case NOR(true, true) => false
    case NOR(true, false) => false
    case NOR(false, true) => false
    case NOR(false, false) => true

  }


  def findType[T](arg: T): String = {

    arg match {
      case _: Boolean => "Argument is a Boolean with value " + arg + "\n"
      case _: Double => "Argument is a Double with value " + arg + "\n"
      case _: Int => "Argument is an Integer with value " + arg + "\n"
      case _: Float => "Argument is a Floating Number with value " + arg + "\n"
      case _: String => "Argument is a String with value " + arg + "\n"
    }
  }


  val bool = true
  val string = "programming"
  val int = 1
  val float = 1.0f
  val double = 1.00

  println("Task 1:")
  println(returnPrimesFromList(List(1, 2, 3, 4, 5, 6, 7, 8, 9)))
  println("Task 2:")
  println(calculator1(Negation(-4)))
  println("Task 3:")
  println(calculator2(Calculator2.Add(4, 5)))
  println("Task 4:")
  println(and(AND(true, false)))
  println(or(OR(true, false)))
  println(xor(XOR(true, false)))
  println(nand(NAND(true, false)))
  println(nor(NOR(true, false)))
  println("Task 5:")
  print(findType(bool))
  print(findType(string))
  print(findType(int))
  print(findType(float))
  print(findType(double))

}
