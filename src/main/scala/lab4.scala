object lab4 extends App {

  val primes = (toN:Int) =>{

    def findPrimes (sieve: List[Int]): List[Int] = sieve match {

      case h::t => h::findPrimes(t filter (x=>x%h != 0))
      case Nil => Nil

    }
    findPrimes(List.range(2,toN))
  }

  def returnPrimes (list: List[Int]): List[Int] =  {

    val listOfPrimes = primes(200)

    list.intersect(listOfPrimes)

  }

  sealed trait Calculator

    case class Addition(x:Int, y:Int) extends Calculator

    case class Negation(x:Int) extends Calculator

    def calculator1(x: Calculator): Int = x match {

      case Addition(x,y) => x+y
      case Negation(x) => -x
    }


  enum Calculator2:
      case Add(x:Int, y:Int)
      case Neg(x:Int)



  def calculator2(x: Calculator2): Int = x match {

    case Calculator2.Add(x, y) => x + y
    case Calculator2.Neg(x) => -x
  }


  def typeOf(arg: AnyVal): String = {
    //pattern match against type of arg
    //the case which matches with type of arg will return the string associated with it
    arg match {
      case _: Boolean => "Argument is a Boolean and Its Value is " + arg + "\n"
      case _: Char => "Argument is a Character and its Value is " + arg + "\n"
      case _: Int => "Argument is an INTEGER and its Value is " + arg + "\n"
      case _: Float => "Argument is a Floating Number and its Value is " + arg + "\n"
      case _: Double => "Argument is a Double and its Value is " + arg + "\n"
    }
  }

  //Main Function

    //Here we have 5 variables of AnyVal type and initiated by value of 5 different types
    val aBool: AnyVal = true
    val aChar: AnyVal = 'a'
    val anInt: AnyVal = 1
    val aFloat: AnyVal = 1.0f
    val aDouble: AnyVal = 1.00

    //Now tyoeOf(arg) method will be called with local variables and print strings returned by it
    print(typeOf(aBool))
    print(typeOf(aChar))
    print(typeOf(anInt))
    print(typeOf(aFloat))
    print(typeOf(aDouble))


  println(returnPrimes(List(1,2,3,4,5,6,7,8,9)))
  println(calculator1(Negation(-4)))
  println(calculator2(Calculator2.Add(4,5)))


}
