package dog
import scala.collection.mutable.HashMap

/**
  * Dog Programming Language
  */
class Dog {
  abstract sealed class DogLine
  case class PrintString(num: Int, s: String) extends DogLine // bark
  case class PromptUser(num: Int) extends DogLine // take
  case class ShowAmt(num: Int) extends DogLine // show
  case class ClearAmt(num: Int) extends DogLine // give
  case class End(num: Int) extends DogLine

  var pc = 0
  var commands = new HashMap[Int, DogLine]
  var labels = new HashMap[String, Int]
  var plates = new Array[Int](10)

  var mouth = 0

  abstract class Dish
  case class Plate(index: Int, name: String) extends Dish {
    def getVal(): Int = plates(index)
    def setVal(amt: Int) = plates(index) = amt
  }

  case class Bowl(index: Int, name: String) extends Dish {
    // Not implemented yet
  }

  // Only holds integers
  var plate0 = new Plate(0, "plate0")
  var plate1 = new Plate(1, "plate1")
  var plate2 = new Plate(2, "plate2")
  var plate3 = new Plate(3, "plate3")
  var plate4 = new Plate(4, "plate4")
  var plate5 = new Plate(5, "plate5")
  var plate6 = new Plate(6, "plate6")
  var plate7 = new Plate(7, "plate7")
  var plate8 = new Plate(8, "plate8")
  var plate9 = new Plate(9, "plate9")

  /*
   * Runtime evaluator
   */
  private def evaluate(line: Int): Unit = {
    commands(line) match {
      case PrintString(_, s: String) =>
        print(s)
        evaluate(line + 1)

      case PromptUser(_) =>
        mouth += scala.io.StdIn.readInt()
        evaluate(line + 1)

      case ShowAmt(_) =>
        print(mouth)
        evaluate(line + 1)

      case ClearAmt(_) =>
        print(mouth)
        mouth = 0
        evaluate(line + 1)

      case End(_) =>
      case _ =>
    }
  }

  object bark {
    def apply(line: String): Unit = {
      commands(pc) = PrintString(pc, line)
      pc += 1
    }
  }

  def take: Unit = {
    commands(pc) = PromptUser(pc)
    pc += 1
  }

  def show: Unit = {
    commands(pc) = ShowAmt(pc)
    pc += 1
  }

  def give: Unit = {
    commands(pc) = ClearAmt(pc)
    pc += 1
  }

  // The line "good boy" executes the program and cannot be omitted
  object good {
    def boy: Unit = {
      commands(pc) = End(pc)
      evaluate(commands.keys.toList.sorted.head)
    }
  }

}
