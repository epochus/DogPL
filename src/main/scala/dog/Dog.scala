package dog

import scala.collection.mutable.{ArrayBuffer, HashMap}
import java.text.DecimalFormat

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
  case class AddAmtc(pos: Int, c: Container, r: Int = 1) extends DogLine // fetch X
  case class Modulus(pos: Int, i: Double, r: Int = 1) extends DogLine // math X
  case class DropAmt(pos: Int, c: Container, r: Int = 1) extends DogLine // drop V
  case class PickUpAmt(pos: Int, c: Container, r: Int = 1) extends DogLine // pickup V
  case class SubAmt(pos: Int, i: Double, r: Int = 1) extends DogLine // eat [X]
  case class SubAmta(pos: Int, i: Double, r: Int = 1) extends DogLine // eat [X]
  case class SubAmtc(pos: Int, c: Container, r: Int = 1) extends DogLine // eat [X]
  case class ClearAmt(pos: Int, c: Container, r: Int = 1) extends DogLine// clear V
  case class Break(pos: Int) extends DogLine // die
  case class Label(pos: Int, s: String) extends DogLine // label label_name
  case class Jump(pos: Int, s: String) extends DogLine // jump label_name
  case class End(num: Int) extends DogLine

  var pc = 0
  var commands = new HashMap[Int, DogLine]
  var labels = new HashMap[String, Int]
  var bowls = new Array[Double](10)
  var safes = new Array[Double](10)

  var mouth: Double = 0
  var floor = new Floor(new ArrayBuffer[Double](10))

  abstract class Container {
    def getVal: Double
    def setVal(amt: Double): Unit

    def jump(label: String): Unit = {
      commands(pc) = Jump(pc, label)
      pc += 1
    }
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
  case class Safe(index: Int, amount: Double) extends Container {
    def getVal(): Double = safes(index)
    def setVal(amt: Double) = safes(index) = amt

  }

  // NOTE: What's the point of 10 bowls here if we have an array that corresponds to the 10 bowls??
  // Only holds doubles
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

  var safe0 = new Safe(0, 0)
  var safe1 = new Safe(1, 0)
  var safe2 = new Safe(2, 0)
  var safe3 = new Safe(3, 0)
  var safe4 = new Safe(4, 0)
  var safe5 = new Safe(5, 0)
  var safe6 = new Safe(6, 0)
  var safe7 = new Safe(7, 0)
  var safe8 = new Safe(8, 0)
  var safe9 = new Safe(9, 0)
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
        var result = new DecimalFormat("0.###").format(mouth)
        for (itr <- 1 to r)
          print(result)
        evaluate(line + 1)

      case GiveAmt(_, r: Int) =>
        var result = new DecimalFormat("0.###").format(mouth)
        for (itr <- 1 to r) {
          print(result)
          mouth = 0
        }
        evaluate(line + 1)

      case AddAmt(_, num: Double, r: Int) =>
        for (itr <- 1 to r)
          mouth += num
        evaluate(line + 1)

      case AddAmtc(_, c: Container, r: Int) =>
        for (itr <- 1 to r)
          mouth += c.getVal
        evaluate(line + 1)

      case Modulus(_, num: Double, r: Int) =>
        for (itr <- 1 to r) {
          mouth %= num
        }
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

      case SubAmta(_, num: Double, r: Int) =>
        mouth = 0
        evaluate(line + 1)

      case SubAmtc(_, c: Container, r: Int) =>
        for (itr <- 1 to r) {
          mouth -= c.getVal
          if (mouth < 0) {
            // handle error
          }
        }
        evaluate(line + 1)

      case ClearAmt(_, c: Container, r: Int) =>
        c.setVal(0)
        evaluate(line + 1)

      // add case for Break()
      // NOTE: I assume this is the equivalent of die/stop?
      case Break(_) =>
        System.exit(0)

      // NOTE: I think these need to be changed
      case Label(_, s: String) =>
        // Does _, in this case, pull in the value passed as the first parameter?
        labels(s) = line
        evaluate(line + 1)

      // NOTE: I think these need to be changed
      case Jump(_, s: String) =>
        val newLine = labels(s)
        evaluate(newLine + 1)

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

    def apply(c: Bowl) = {
      commands(pc) = AddAmtc(pc, c, 1)
      pc += 1
    }
  }

  def math(d: Double): Unit = {
    commands(pc) = Modulus(pc, d)
    pc += 1
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

  object eat {
    def apply(): Unit = {
      commands(pc) = SubAmta(pc, mouth)
      pc += 1
    }
    def apply(x: Int) = {
      commands(pc) = SubAmt(pc, x)
      pc += 1
    }

    def apply(c: Bowl) = {
      commands(pc) = SubAmtc(pc, c, 1)
      pc += 1
    }
  }


  object clear {
    def apply(c: Container)= {
      commands(pc) = ClearAmt(pc, c)
      pc += 1
    }
  }

  def label(label: String): Unit = {
    commands(pc) = Label(pc, label)
    pc += 1
  }

  def jump(label: String): Unit = {
    commands(pc) = Jump(pc, label)
    pc += 1
  }

  def stop: Unit = {
    commands(pc) = Break(pc)
    // No point in incrementing pc if this stops the program.
  }



  // The line "good boy" executes the program and cannot be omitted
  object good {
    def boy: Unit = {
      commands(pc) = End(pc)
      evaluate(commands.keys.toList.sorted.head)
    }
  }


  // Repetitive ... Not sure of a better way to do it
  // This doesn't take into account negative numbers, which should execute the command 0 times.
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

    def math(d: Double): Unit = {
      commands(pc) = Modulus(pc, d, num)
      pc += 1
    }

    object fetch {
      def apply(x: Int) = {
        commands(pc) = AddAmt(pc, x, num)
        pc += 1
      }

      def apply(c: Bowl) = {
        commands(pc) = AddAmtc(pc, c, num)
        pc += 1
      }
    }

    object eat {
      def apply(x: Int) = {
        commands(pc) = SubAmt(pc, x, num)
        pc += 1
      }

      def apply(c: Bowl) = {
        commands(pc) = SubAmtc(pc, c, num)
        pc += 1
      }
    }

    def label(label: String): Unit = {
      commands(pc) = Label(pc, label)
      pc += 1
    }

    object jump {
      def apply(label: String): Unit = {
        commands(pc) = Jump(pc, label)
        pc += 1
      }
    }
    /*
    def jump(label: String): Unit = {
      commands(pc) = Jump(pc, label)
      pc += 1
    }
    */

  }

  implicit def numToRepeat(num: Int) = Repeat(num)
  implicit def varToRepeat(c: Container) = {
    // NOTE: I'm not sure I implemented this correctly... namely, the case classes part.
    c match {
      case Bowl(_, amt) =>
        if (c.getVal <= 0) {
          // NOTE: not sure if I should just repeat 0 times or just increment the pc
          Repeat(0)
        } else {
          Repeat(Math.floor(c.getVal).toInt)
        }

      case Floor(_) =>
        Repeat(Math.floor(c.getVal).toInt)

      case Safe(_, amt) =>
        if (c.getVal > 0) {
          Repeat(1)
        } else if (c.getVal == 0) {
          // NOTE: not sure if I should just repeat 0 times or just increment the pc
          Repeat(0)
        }
    }
  }
  implicit def funcToRepeat(func: Unit) = Repeat(1)

}