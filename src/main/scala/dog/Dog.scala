package dog
import scala.collection.mutable.HashMap

/**
  * Dog Programming Language
  */
class Dog {
  abstract sealed class DogLine
  case class PrintString(pos: Int, s: String, r: Int = 1) extends DogLine // bark
  case class PromptUser(pos: Int, r: Int = 1) extends DogLine // take
  case class ShowAmt(pos: Int, r: Int = 1) extends DogLine // show
  case class GiveAmt(pos: Int, r: Int = 1) extends DogLine // give
  case class AddAmt(pos: Int, i: Int, r: Int = 1) extends DogLine // fetch X
  case class DropAmt(pos: Int, c: Container, r: Int = 1) extends DogLine // drop V
  case class PickUpAmt(pos: Int, c: Container, r: Int = 1) extends DogLine // pickup V
  case class SubAmt(pos: Int, i: Int = mouth, r: Int = 1) extends DogLine // eat [X]
  case class ClearAmt(pos: Int, c: Container, r: Int = 1) extends DogLine// clear V
  case class Break(pos: Int) extends DogLine // die
  case class Label(pos: Int, s: String) extends DogLine // label label_name
  case class Jump(pos: Int, s: String) extends DogLine // jump label_name
  case class End(num: Int) extends DogLine

  var pc = 0
  var commands = new HashMap[Int, DogLine]
  var labels = new HashMap[String, Int]
  var plates = new Array[Int](10)

  // plate -> safe
  // loop -> routine
  // die -> stop (equivalent to break statement)

  var mouth = 0

  abstract class Container {
    def getVal: Int
    def setVal(amt: Int): Unit
  }
  case class Plate(index: Int, amount: Int) extends Container {
    def getVal(): Int = plates(index)
    def setVal(amt: Int) = plates(index) = amt
  }

  /*
  case class Bowl(index: Int, name: String) extends Container {
    // Not implemented yet
  }
  */

  // Only holds integers
  var plate0 = new Plate(0, 0)
  var plate1 = new Plate(1, 0)
  var plate2 = new Plate(2, 0)
  var plate3 = new Plate(3, 0)
  var plate4 = new Plate(4, 0)
  var plate5 = new Plate(5, 0)
  var plate6 = new Plate(6, 0)
  var plate7 = new Plate(7, 0)
  var plate8 = new Plate(8, 0)
  var plate9 = new Plate(9, 0)


  /*
   * Runtime evaluator
   */
  private def evaluate(line: Int): Unit = {
    commands(line) match {
      case PrintString(_, s: String, r: Int) =>
        print("pol")
        for (itr <- 1 to r)
          print(s)
        evaluate(line + 1)

      case PromptUser(_, r: Int) =>
        for (itr <- 1 to r)
          mouth += scala.io.StdIn.readInt()
        evaluate(line + 1)

      case ShowAmt(_, r: Int) =>
        for (itr <- 1 to r)
          print(mouth)
        evaluate(line + 1)

      case GiveAmt(_, r: Int) =>
        for (itr <- 1 to r) {
          print(mouth)
          mouth = 0
        }
        evaluate(line + 1)

      case AddAmt(_, num: Int, r: Int) =>
        for (itr <- 1 to r)
          mouth += num
        evaluate(line + 1)

      case DropAmt(_, c: Container, r: Int) =>
        for (itr <- 1 to r) {
          val total = c.getVal + mouth
          mouth = 0
          c.setVal(total)
        }
        evaluate(line + 1)

      case PickUpAmt(_, c: Container, r: Int) =>
        mouth += c.getVal
        c.setVal(0)
        evaluate(line + 1)

      case SubAmt(_, num: Int, r: Int) =>
        for (itr <- 1 to r) {
          mouth -= num
          if (mouth < 0) {
            // handle error
          }
        }
        evaluate(line + 1)

      case ClearAmt(_, c: Container, r: Int) =>
        c.setVal(0)
        evaluate(line + 1)

      // add case for Break()

      case Label(_, s: String) =>
        evaluate(line + 1)

      case Jump(_, s: String) =>
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

  def take = {
    commands(pc) = PromptUser(pc)
    pc += 1
  }

  def show: Unit = {
    commands(pc) = ShowAmt(pc)
    pc += 1
  }

  def give = {
    commands(pc) = GiveAmt(pc)
    pc += 1
  }

  object fetch {
    def apply(x: Int) = {
      commands(pc) = AddAmt(pc, x)
      pc += 1
    }

    def apply(x: Plate) = {
      commands(pc) = AddAmt(pc, x.amount, 2)
      x.setVal(0)
      pc += 1
    }
  }

  object drop {
    def apply(c: Container)= {
      commands(pc) = DropAmt(pc, c)
      pc += 1
    }
  }

  object pickup {
    def apply(c: Container)= {
      commands(pc) = DropAmt(pc, c)
      pc += 1
    }
  }

  def eat(x: Int = mouth) = {
    commands(pc) = SubAmt(pc, x)
    pc += 1
  }

  object clear {
    def apply(c: Container)= {
      commands(pc) = ClearAmt(pc, c)
      pc += 1
    }
  }


  // The line "good boy" executes the program and cannot be omitted
  object good {
    def boy: Unit = {
      commands(pc) = End(pc)
      evaluate(commands.keys.toList.sorted.head)
    }
  }

  // Repetitive ... Not sure of a better way to do it
  case class Repeat(num: Int) {

    object bark {
      def apply(str: String): Unit = {
        commands(pc) = PrintString(pc, str, num)
        pc += 1
      }
    }

    def take = {
      commands(pc) = PromptUser(pc, num)
      pc += 1
    }

    def show: Unit = {
      commands(pc) = ShowAmt(pc, num)
      pc += 1
    }

    def give: Unit = {
      commands(pc) = GiveAmt(pc, num)
      pc += 1
    }

    object fetch {
      def apply(x: Int) = {
        commands(pc) = AddAmt(pc, x, num)
        pc += 1
      }

      def apply(x: Plate) = {
        commands(pc) = AddAmt(pc, x.amount, num)
        x.setVal(0)
        pc += 1
      }
    }

  }

  implicit def numToRepeat(num: Int) = Repeat(num)
  implicit def varToRepeat(num: Plate) = Repeat(num.amount)
}