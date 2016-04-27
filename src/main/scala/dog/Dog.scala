package dog
import scala.collection.mutable.{ArrayBuffer, HashMap}

/**
  * Dog Programming Language
  */
class Dog {
  abstract sealed class DogLine
  case class PrintString(pos: Int, s: String, r: Int = 1) extends DogLine // bark
  case class PromptUser(pos: Int, r: Int = 1) extends DogLine // take
  case class ShowAmt(pos: Int, r: Int = 1) extends DogLine // show
  case class GiveAmt(pos: Int, r: Int = 1) extends DogLine // give
  case class AddAmt(pos: Int, i: Double, r: Int = 1) extends DogLine // fetch X
  case class DropAmt(pos: Int, c: Container, r: Int = 1) extends DogLine // drop V
  case class PickUpAmt(pos: Int, c: Container, r: Int = 1) extends DogLine // pickup V
  case class SubAmt(pos: Int, i: Double = mouth, r: Int = 1) extends DogLine // eat [X]
  case class ClearAmt(pos: Int, c: Container, r: Int = 1) extends DogLine// clear V
  case class Break(pos: Int) extends DogLine // die
  case class Label(pos: Int, s: String) extends DogLine // label label_name
  case class Jump(pos: Int, s: String) extends DogLine // jump label_name
  case class End(num: Int) extends DogLine

  var pc = 0
  var commands = new HashMap[Int, DogLine]
  var labels = new HashMap[String, Int]
  var bowls = new Array[Double](10)

  // plate -> safe
  // loop -> routine
  // die -> stop (equivalent to break statement)

  var mouth: Double = 0
  var floor = new Floor(new ArrayBuffer[Double](10))

  abstract class Container {
    def getVal: Double
    def setVal(amt: Double): Unit
  }
  case class Floor(arrayBuffer: ArrayBuffer[Double]) extends Container {
    def getVal: Double = {
      val random = scala.util.Random

      if (arrayBuffer.length == 0) {
        return (random.nextDouble() * 10) + 1
      }

      val index = random.nextInt(arrayBuffer.length)
      val returnVal = arrayBuffer(index)

      if (returnVal == 0) {
        return (random.nextDouble() * 10) + 1
      }

      arrayBuffer -= returnVal

      return returnVal
    }
    def setVal(amt:Double): Unit = {
      arrayBuffer += amt
    }
  }
  case class Bowl(index: Int, amount: Double) extends Container {
    def getVal(): Double = bowls(index)
    def setVal(amt: Double) = bowls(index) = amt
  }

  // Only holds integers
  var bowl0 = new Bowl(0, 0)
  var bowl1 = new Bowl(1, 0)
  var bowl2 = new Bowl(2, 0)
  var bowl3 = new Bowl(3, 0)
  var bowl4 = new Bowl(4, 0)
  var bowl5 = new Bowl(5, 0)
  var bowl6 = new Bowl(6, 0)
  var bowl7 = new Bowl(7, 0)
  var bowl8 = new Bowl(8, 0)
  var bowl9 = new Bowl(9, 0)

  /*
   * Runtime evaluator
   */
  private def evaluate(line: Int): Unit = {
    commands(line) match {
      case PrintString(_, s: String, r: Int) =>
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

      case AddAmt(_, num: Double, r: Int) =>
        for (itr <- 1 to r)
          mouth += num
        evaluate(line + 1)

      case DropAmt(_, c: Container, r: Int) =>
        if (c.isInstanceOf[Floor]) {
          c.setVal(mouth)
          mouth = 0
        } else {
          for (itr <- 1 to r) {
            val total = c.getVal + mouth
            mouth = 0
            c.setVal(total)
          }
        }
        evaluate(line + 1)

      case PickUpAmt(_, c: Container, r: Int) =>
        mouth += c.getVal
        if (!c.isInstanceOf[Floor]) {
          c.setVal(0)
        }
        evaluate(line + 1)

      case SubAmt(_, num: Double, r: Int) =>
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
      //print("not here")
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

  def give: Unit = {
    commands(pc) = GiveAmt(pc)
    pc += 1
  }

  object fetch {
    def apply(x: Int) = {
      commands(pc) = AddAmt(pc, x)
      pc += 1
    }

    def apply(x: Bowl) = {
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
      commands(pc) = PickUpAmt(pc, c)
      pc += 1
    }
  }

  def eat(x: Double = mouth) = {
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
        //print("1")
      }
    }

    def take() = {
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

      def apply(x: Bowl) = {
        commands(pc) = AddAmt(pc, x.amount, num)
        x.setVal(0)
        pc += 1
      }
    }

  }

  implicit def numToRepeat(num: Int) = Repeat(num)
  implicit def varToRepeat(num: Bowl) = Repeat(Math.floor(num.amount).toInt)
  implicit def funcToRepeat(func: Unit) = Repeat(1)

}