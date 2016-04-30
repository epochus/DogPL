package dog

import java.lang.invoke.MethodHandles

import scala.collection.mutable.{ArrayBuffer, HashMap}
import java.text.DecimalFormat

import scala.math.Equiv

/**
  * Dog Programming Language
  */
class Dog {
  abstract sealed class DogLine
  case class PrintString(pos: Int, referencedRepeat: Container, s: String, explicitRepeat: Int = 1) extends DogLine // bark
  case class PromptUser(pos: Int, referencedRepeat: Container, explicitRepeat: Int = 1) extends DogLine // take
  case class ShowAmt(pos: Int, referencedRepeat: Container, explicitRepeat: Int = 1) extends DogLine // show
  case class GiveAmt(pos: Int, referencedRepeat: Container, explicitRepeat: Int = 1) extends DogLine // give
  case class AddAmt(pos: Int, referencedRepeat: Container, i: Double, explicitRepeat: Int = 1) extends DogLine // fetch X
  case class AddAmtc(pos: Int, referencedRepeat: Container, c: Container, explicitRepeat: Int = 1) extends DogLine // fetch X
  case class Modulus(pos: Int, referencedRepeat: Container, i: Double, explicitRepeat: Int = 1) extends DogLine // math X
  case class DropAmt(pos: Int, referencedRepeat: Container, c: Container, explicitRepeat: Int = 1) extends DogLine // drop V
  case class PickUpAmt(pos: Int, referencedRepeat: Container, c: Container, explicitRepeat: Int = 1) extends DogLine // pickup V
  case class SubAmt(pos: Int, referencedRepeat: Container, i: Double, explicitRepeat: Int = 1) extends DogLine // eat [X]
  case class SubAmta(pos: Int, referencedRepeat: Container, i: Double, explicitRepeat: Int = 1) extends DogLine // eat [X]
  case class SubAmtc(pos: Int, referencedRepeat: Container, c: Container, explicitRepeat: Int = 1) extends DogLine // eat [X]
  case class ClearAmt(pos: Int, referencedRepeat: Container, c: Container, explicitRepeat: Int = 1) extends DogLine// clear V
  case class Exit(pos: Int, referencedRepeat: Container) extends DogLine // die
  case class Label(pos: Int, referencedRepeat: Container, s: String) extends DogLine // label label_name
  case class Jump(pos: Int, referencedRepeat: Container, s: String, explicitRepeat: Int = 1) extends DogLine // jump label_name
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
    def repeat(): Int = {
      if (getVal <= 0) {
        return 0
      } else {
        return getVal.toInt;
      }
    }

    def bark(line: String): Unit = {
      commands(pc) = PrintString(pc, this, line)
      pc += 1
    }

    def take = {
      commands(pc) = PromptUser(pc, this)
      pc += 1
    }

    def show: Unit = {
      commands(pc) = ShowAmt(pc, this)
      pc += 1
    }

    def give: Unit = {
      commands(pc) = GiveAmt(pc, this)
      pc += 1
    }

    def fetch(x: Int) = {
      commands(pc) = AddAmt(pc, this, x)
      pc += 1
    }

    def fetch(c: Container) = {
      commands(pc) = AddAmtc(pc, this, c, 1)
      pc += 1
    }

    def math(d: Double): Unit = {
      commands(pc) = Modulus(pc, this, d)
      pc += 1
    }

    def drop(c: Container)= {
      commands(pc) = DropAmt(pc, this, c)
      pc += 1
    }

    def pickup(c: Container)= {
      commands(pc) = PickUpAmt(pc, this, c)
      pc += 1
    }

    def eat(): Unit = {
      commands(pc) = SubAmta(pc, this, mouth)
      pc += 1
    }
    def eat(x: Int) = {
      commands(pc) = SubAmt(pc, this, x)
      pc += 1
    }
    def eat(c: Container) = {
      commands(pc) = SubAmtc(pc, this, c, 1)
      pc += 1
    }


    def clear(c: Container)= {
      commands(pc) = ClearAmt(pc, this, c)
      pc += 1
    }

    def label(label: String): Unit = {
      commands(pc) = Label(pc, this, label)
      pc += 1
    }

    def jump(label: String): Unit = {
      commands(pc) = Jump(pc, this, label)
      pc += 1
    }

    def stop: Unit = {
      commands(pc) = Exit(pc, this)
      // No point in incrementing pc if this stops the program.
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
    override def repeat(): Int = {
      if (getVal <= 0) {
        return 0
      } else {
        return 1
      }
    }
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
      case PrintString(_, referencedRepeat: Container, s: String, explicitRepeat: Int) =>
        if (referencedRepeat != null) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            print(s)
          }
        } else {
          for (itr <- 1 to explicitRepeat)
            print(s)
        }
        evaluate(line + 1)

      case PromptUser(_, referencedRepeat: Container, explicitRepeat: Int) =>
        if (referencedRepeat != null) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            mouth += scala.io.StdIn.readInt()
          }
        } else {
          for (itr <- 1 to explicitRepeat)
            mouth += scala.io.StdIn.readInt()
        }
        evaluate(line + 1)

      case ShowAmt(_, referencedRepeat: Container, explicitRepeat: Int) =>
        var result = new DecimalFormat("0.###").format(mouth)
        if (referencedRepeat != null) {
          for (iter <- 1 to explicitRepeat) {
            print(result)
          }
        } else {
          for (itr <- 1 to explicitRepeat)
            print(result)
        }
        evaluate(line + 1)

      case GiveAmt(_, referencedRepeat: Container, explicitRepeat: Int) =>
        var result = new DecimalFormat("0.###").format(mouth)
        if (referencedRepeat != null) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            print(result)
            mouth = 0
          }
        } else {
          for (itr <- 1 to explicitRepeat) {
            print(result)
            mouth = 0
          }
        }
        evaluate(line + 1)

      case AddAmt(_, referencedRepeat: Container, num: Double, explicitRepeat: Int) =>
        if (referencedRepeat != null) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            mouth += num
          }
        } else {
          for (itr <- 1 to explicitRepeat)
            mouth += num
        }
        evaluate(line + 1)

      case AddAmtc(_, referencedRepeat: Container, c: Container, explicitRepeat: Int) =>
        if (referencedRepeat != null) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            mouth += c.getVal
          }
        } else {
          for (itr <- 1 to explicitRepeat)
            mouth += c.getVal
        }
        evaluate(line + 1)

      case Modulus(_, referencedRepeat: Container, num: Double, explicitRepeat: Int) =>
        if (referencedRepeat != null) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            mouth %= num
          }
        } else {
          for (itr <- 1 to explicitRepeat) {
            mouth %= num
          }
        }
        evaluate(line + 1)

      case DropAmt(_, referencedRepeat: Container, c: Container, explicitRepeat: Int) =>
        if (referencedRepeat != null) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            if (c.isInstanceOf[Floor]) {
              c.setVal(mouth)
              mouth = 0
            } else {
              val total = c.getVal + mouth
              mouth = 0
              c.setVal(total)
            }
          }
        } else {
          if(c.isInstanceOf[Floor]) {
            c.setVal(mouth)
            mouth = 0
          } else {
            for (itr <- 1 to explicitRepeat) {
              val total = c.getVal + mouth
              mouth = 0
              c.setVal(total)
            }
          }
        }
        evaluate(line + 1)

      case PickUpAmt(_, referencedRepeat: Container, c: Container, explicitRepeat: Int) =>
        if (referencedRepeat != null) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            mouth += c.getVal
            if (!c.isInstanceOf[Floor]) {
              c.setVal(0)
            }
          }
        } else {
          for (iter <- 1 to explicitRepeat) {
            mouth += c.getVal
            if (!c.isInstanceOf[Floor]) {
              c.setVal(0)
            }
          }
        }
        evaluate(line + 1)

      case SubAmt(_, referencedRepeat: Container, num: Double, explicitRepeat: Int) =>
        if (referencedRepeat != null) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            mouth -= num
            if (mouth < 0) {
              // handle error
            }
          }
        } else {
          for (itr <- 1 to explicitRepeat) {
            mouth -= num
            if (mouth < 0) {
              // handle error
            }
          }
        }
        evaluate(line + 1)

      case SubAmta(_, referencedRepeat: Container, num: Double, explicitRepeat: Int) =>
        if (referencedRepeat != null) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            mouth = 0
          }
        } else {
          for (iter <- 1 to explicitRepeat) {
            mouth = 0
          }
        }
        evaluate(line + 1)

      case SubAmtc(_, referencedRepeat: Container, c: Container, explicitRepeat: Int) =>
        if (referencedRepeat != null) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            mouth -= c.getVal
            if (mouth < 0) {
              // handle error
            }
          }
        } else {
          for (itr <- 1 to explicitRepeat) {
            mouth -= c.getVal
            if (mouth < 0) {
              // handle error
            }
          }
        }
        evaluate(line + 1)

      case ClearAmt(_, referencedRepeat: Container, c: Container, explicitRepeat: Int) =>
        if (referencedRepeat != null) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            c.setVal(0)
          }
        } else {
          for (iter <- 1 to explicitRepeat) {
            c.setVal(0)
          }
        }
        evaluate(line + 1)

      case Exit(_, referencedRepeat: Container) =>
        if (referencedRepeat != null) {
          if (referencedRepeat.repeat() > 0) {
            System.exit(0)
          }
        } else {
          System.exit(0)
        }

      case Label(_, referencedRepeat: Container, s: String) =>
        if (referencedRepeat != null && referencedRepeat.repeat() > 0) {
          labels(s) = line
        } else {
          labels(s) = line
        }
        evaluate(line + 1)

      case Jump(_, referencedRepeat: Container, s: String, explicitRepeat: Int) =>
        if (referencedRepeat != null) {
          if (referencedRepeat.repeat() > 0) {
            val newLine = labels(s)
            evaluate(newLine + 1)
          } else {
            evaluate(line + 1)
          }
        } else {
          val newLine = labels(s)
          evaluate(newLine + 1)
        }

      case End(_) =>
      case _ =>
    }
  }

  object bark {
    def apply(line: String): Unit = {
      commands(pc) = PrintString(pc, null, line)
      pc += 1
    }
  }

  def take = {
    commands(pc) = PromptUser(pc, null)
    pc += 1
  }

  def show: Unit = {
    commands(pc) = ShowAmt(pc, null)
    pc += 1
  }

  def give: Unit = {
    commands(pc) = GiveAmt(pc, null)
    pc += 1
  }

  object fetch {
    def apply(x: Int) = {
      commands(pc) = AddAmt(pc, null, x)
      pc += 1
    }

    def apply(c: Container) = {
      commands(pc) = AddAmtc(pc, null, c, 1)
      pc += 1
    }
  }

  def math(d: Double): Unit = {
    commands(pc) = Modulus(pc, null, d)
    pc += 1
  }

  object drop {
    def apply(c: Container)= {
      commands(pc) = DropAmt(pc, null, c)
      pc += 1
    }
  }

  object pickup {
    def apply(c: Container)= {
      commands(pc) = PickUpAmt(pc, null, c)
      pc += 1
    }
  }

  object eat {
    def apply(): Unit = {
      commands(pc) = SubAmta(pc, null, mouth)
      pc += 1
    }
    def apply(x: Int) = {
      commands(pc) = SubAmt(pc, null, x)
      pc += 1
    }

    def apply(c: Container) = {
      commands(pc) = SubAmtc(pc, null, c, 1)
      pc += 1
    }
  }


  object clear {
    def apply(c: Container)= {
      commands(pc) = ClearAmt(pc, null, c)
      pc += 1
    }
  }

  def label(label: String): Unit = {
    commands(pc) = Label(pc, null, label)
    pc += 1
  }

  def jump(label: String): Unit = {
    commands(pc) = Jump(pc, null, label)
    pc += 1
  }

  def stop: Unit = {
    commands(pc) = Exit(pc, null)
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
    /*
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
  */

  implicit def numToRepeat(num: Int) = {
    object bark {
      def apply(line: String): Unit = {
        commands(pc) = PrintString(pc, null, line, num)
        pc += 1
      }
    }

    def take = {
      commands(pc) = PromptUser(pc, null, num)
      pc += 1
    }

    def show: Unit = {
      commands(pc) = ShowAmt(pc, null, num)
      pc += 1
    }

    def give: Unit = {
      commands(pc) = GiveAmt(pc, null, num)
      pc += 1
    }

    object fetch {
      def apply(x: Int) = {
        commands(pc) = AddAmt(pc, null, x, num)
        pc += 1
      }

      def apply(c: Container) = {
        commands(pc) = AddAmtc(pc, null, c, num)
        pc += 1
      }
    }

    def math(d: Double): Unit = {
      commands(pc) = Modulus(pc, null, d, num)
      pc += 1
    }

    object drop {
      def apply(c: Container) = {
        commands(pc) = DropAmt(pc, null, c, num)
        pc += 1
      }
    }

    object pickup {
      def apply(c: Container) = {
        commands(pc) = PickUpAmt(pc, null, c, num)
        pc += 1
      }
    }

    object eat {
      def apply(): Unit = {
        commands(pc) = SubAmta(pc, null, mouth, num)
        pc += 1
      }

      def apply(x: Int) = {
        commands(pc) = SubAmt(pc, null, x, num)
        pc += 1
      }

      def apply(c: Container) = {
        commands(pc) = SubAmtc(pc, null, c, num)
        pc += 1
      }
    }


    object clear {
      def apply(c: Container) = {
        commands(pc) = ClearAmt(pc, null, c, num)
        pc += 1
      }
    }

    def label(label: String): Unit = {
      commands(pc) = Label(pc, null, label)
      pc += 1
    }

    def jump(label: String): Unit = {
      commands(pc) = Jump(pc, null, label, num)
      pc += 1
    }

    def stop: Unit = {
      commands(pc) = Exit(pc, null)
      // No point in incrementing pc if this stops the program.
    }
  }

    /*
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
  */

}
