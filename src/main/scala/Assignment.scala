import com.sun.istack.internal.logging.Logger

class Operation {

  def pascalTriangle(c: Int, r: Int): Int = {
    if (c == 0 || c == r) {
      1
    }
    else pascalTriangle(c - 1, r - 1) + pascalTriangle(c, r - 1)
  }

  def selectOptionForInt(option: String, first: Int, second: Int): String = {
    option match {
      case "squares" => s"The Result Is : ${higherOrderForInt(first, second, (first, second) => first * second + second * second)}"
      case "ints" => s"The Result Is : ${higherOrderForInt(first, second, (first, second) => first + second)}"
      case "cubes" => s"The Result Is : ${higherOrderForInt(first, second, (first, second) => ((first * first * first) + (second * second * second)))}"
      case _ => "Illegal Operation"
    }
  }

  private def higherOrderForInt(first: Int, second: Int, f: (Int, Int) => Int): Int = {
    f(first, second)
  }

  def selectOptionForList(number: List[Int], option: String, f: (List[Int]) => Int): Int = {
    option match {
      case "sum" => higherOrderForList(number, f(number) => case head :: tail => head + f(tail)
    case Nil => 0
    }
    case "product"
    => higherOrderForList()
    case "max"
    => higherOrderForList()
    case _ => "Illegal Operation"
  }

  private def higherOrderForList(number: List[Int], f: (List[Int]) => Int): Int = {
    f(number)
  }
}

}

object Operation extends App {
  val log = Logger.getLogger(this.getClass)
  val obj = new Operation
  val temp = 1 to 10
  val test1: Int = 20
  val test2: Int = 5
  val testList1 = temp.toList
  val operator: String = ""

  log.info(s"Question 1 : ${obj.selectOptionForInt(operator, test1, test2)}")
  log.info(s"Question 2 : ${obj.selectOptionForInt(operator, test1, test2)}")
  log.info(s"Question 3 : ${obj.pascalTriangle(0, test2)}")
}