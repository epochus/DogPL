package dog

import scala.StringBuilder
import scala.collection.mutable
import java.text.DecimalFormat

/**
  * Dog Programming Language
  */
class Dog {
  abstract sealed class DogLine
  case class BarkS(pos: Int, s: String, explicitRepeat: Int = 1, referencedRepeat: Container = Empty()) extends DogLine
  case class PromptUser(pos: Int, explicitRepeat: Int = 1, referencedRepeat: Container = Empty()) extends DogLine
  case class PrintMouth(pos: Int, explicitRepeat: Int = 1, referencedRepeat: Container = Empty()) extends DogLine
  case class ClearMouth(pos: Int, explicitRepeat: Int = 1, referencedRepeat: Container = Empty()) extends DogLine
  case class FetchN(pos: Int, i: Double, explicitRepeat: Int = 1, referencedRepeat: Container = Empty()) extends DogLine
  case class FetchV(pos: Int, c: Container, explicitRepeat: Int = 1, referencedRepeat: Container = Empty()) extends DogLine
  case class DropV(pos: Int, c: Container, explicitRepeat: Int = 1, referencedRepeat: Container = Empty()) extends DogLine
  case class PickUpV(pos: Int, c: Container, explicitRepeat: Int = 1, referencedRepeat: Container = Empty()) extends DogLine
  case class EatAll(pos: Int, i: Double, explicitRepeat: Int = 1, referencedRepeat: Container = Empty()) extends DogLine
  case class EatN(pos: Int, i: Double, explicitRepeat: Int = 1, referencedRepeat: Container = Empty()) extends DogLine
  case class EatV(pos: Int, c: Container, explicitRepeat: Int = 1, referencedRepeat: Container = Empty()) extends DogLine
  case class ClearV(pos: Int, c: Container, explicitRepeat: Int = 1, referencedRepeat: Container = Empty()) extends DogLine
  case class Exit(pos: Int, referencedRepeat: Container = Empty()) extends DogLine
  case class RoutineS(pos: Int, s: String, referencedRepeat: Container = Empty()) extends DogLine
  case class JumpS(pos: Int, s: String, explicitRepeat: Int = 1, referencedRepeat: Container = Empty()) extends DogLine
  case class SaveLeftoversN(pos: Int, i: Double, explicitRepeat: Int = 1, referencedRepeat: Container = Empty()) extends DogLine
  case class Memorize(value: String, explicitRepeat: Int = 1, referencedRepeat: Container = Empty()) extends DogLine
  case class Talk(explicitRepeat: Int = 1, referencedRepeat: Container = Empty()) extends DogLine

  case class End(num: Int) extends DogLine

  var pc = 0
  var commands = new mutable.HashMap[Int, DogLine]
  var labels = new mutable.HashMap[String, Int]
  var bowls = new Array[Double](10)
  var safes = new Array[Double](10)

  var mouth = 0.0
  var floor = new Floor(new mutable.ArrayBuffer[Double](10))
  var string = new StringBuilder()

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
   * Contains methods that execute when a container variable precedes a command
   */
  abstract class Container {
    def getVal: Double
    def setVal(amt: Double): Unit
    def repeat(): Int = {
      if (getVal <= 0) {
        0
      } else {
        getVal.toInt
      }
    }

    def bark(line: String): Unit = {
      commands(pc) = BarkS(pc, line, 1, this)
      pc += 1
    }

    def take(): Unit = {
      commands(pc) = PromptUser(pc, 1, this)
      pc += 1
    }

    def show(): Unit = {
      commands(pc) = PrintMouth(pc, 1, this)
      pc += 1
    }

    def give(): Unit = {
      commands(pc) = ClearMouth(pc, 1, this)
      pc += 1
    }

    def fetch(x: Int) = {
      commands(pc) = FetchN(pc, x, 1, this)
      pc += 1
    }

    def fetch(c: Container) = {
      commands(pc) = FetchV(pc, c, 1, this)
      pc += 1
    }

    def drop(c: Container)= {
      commands(pc) = DropV(pc, c, 1, this)
      pc += 1
    }

    def pickup(c: Container)= {
      commands(pc) = PickUpV(pc, c, 1, this)
      pc += 1
    }

    def eat(): Unit = {
      commands(pc) = EatAll(pc, mouth, 1, this)
      pc += 1
    }

    def eat(x: Int) = {
      commands(pc) = EatN(pc, x, 1, this)
      pc += 1
    }

    def eat(c: Container) = {
      commands(pc) = EatV(pc, c, 1, this)
      pc += 1
    }

    def clear(c: Container)= {
      commands(pc) = ClearV(pc, c, 1, this)
      pc += 1
    }

    def die(): Unit = {
      commands(pc) = Exit(pc, this)
      pc += 1
    }

    def routine(label: String): Unit = {
      commands(pc) = RoutineS(pc, label, this)
      pc += 1
    }

    def jump(label: String): Unit = {
      commands(pc) = JumpS(pc, label, 1, this)
      pc += 1
    }

    def saveLeftovers(d: Double): Unit = {
      commands(pc) = SaveLeftoversN(pc, d, 1, this)
      pc += 1
    }
  }

  case class Floor(arrayBuffer: mutable.ArrayBuffer[Double]) extends Container {
    def getVal: Double = {
      val random = scala.util.Random

      if (arrayBuffer.isEmpty) {
        (random.nextDouble() * 10) + 1
      }

      val index = random.nextInt(arrayBuffer.length)
      val returnVal = arrayBuffer(index)

      if (returnVal == 0) {
        (random.nextDouble() * 10) + 1
      }

      arrayBuffer -= returnVal

      returnVal
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
        0
      } else {
        1
      }
    }
  }

  case class Empty() extends Container {
    def getVal(): Double = 0
    def setVal(amt: Double): Unit = print("empty")
  }

  /*
   * Contains methods that execute when a number precedes a command
   */
  implicit def numToRepeat(num: Int) = Repeat(num)
  case class Repeat(num: Int) {
    object bark {
      def apply(line: String): Unit = {
        commands(pc) = BarkS(pc, line, num)
        pc += 1
      }
    }

    def take() = {
      commands(pc) = PromptUser(pc, num)
      pc += 1
    }

    def show(): Unit = {
      commands(pc) = PrintMouth(pc, num)
      pc += 1
    }

    def give(): Unit = {
      commands(pc) = ClearMouth(pc, num)
      pc += 1
    }

    object fetch {
      def apply(x: Int) = {
        commands(pc) = FetchN(pc, x, num)
        pc += 1
      }

      def apply(c: Container) = {
        commands(pc) = FetchV(pc, c, num)
        pc += 1
      }
    }

    object drop {
      def apply(c: Container) = {
        commands(pc) = DropV(pc, c, num)
        pc += 1
      }
    }

    object pickup {
      def apply(c: Container) = {
        commands(pc) = PickUpV(pc, c, num)
        pc += 1
      }
    }

    object eat {
      def apply(): Unit = {
        commands(pc) = EatAll(pc, mouth, num)
        pc += 1
      }

      def apply(x: Int) = {
        commands(pc) = EatN(pc, x, num)
        pc += 1
      }

      def apply(c: Container) = {
        commands(pc) = EatV(pc, c, num)
        pc += 1
      }
    }

    object clear {
      def apply(c: Container) = {
        commands(pc) = ClearV(pc, c, num)
        pc += 1
      }
    }

    def die(): Unit = {
      commands(pc) = Exit(pc)
      pc += 1
    }

    def routine(label: String): Unit = {
      commands(pc) = RoutineS(pc, label)
      pc += 1
    }

    def jump(label: String): Unit = {
      commands(pc) = JumpS(pc, label, num)
      pc += 1
    }

    def saveLeftovers(d: Double): Unit = {
      commands(pc) = SaveLeftoversN(pc, d, num)
      pc += 1
    }
  }

  /*
   *  Methods that execute when nothing precedes a command
   */
  object bark {
    def apply(line: String): Unit = {
      commands(pc) = BarkS(pc, line)
      pc += 1
    }
  }

  def take() = {
    commands(pc) = PromptUser(pc)
    pc += 1
  }

  def show(): Unit = {
    commands(pc) = PrintMouth(pc)
    pc += 1
  }

  def give: Unit = {
    commands(pc) = ClearMouth(pc)
    pc += 1
  }

  object fetch {
    def apply(x: Int) = {
      commands(pc) = FetchN(pc, x, 1)
      pc += 1
    }

    def apply(c: Container) = {
      commands(pc) = FetchV(pc, c, 1)
      pc += 1
    }
  }

  object drop {
    def apply(c: Container)= {
      commands(pc) = DropV(pc, c)
      pc += 1
    }
  }

  object pickup {
    def apply(c: Container)= {
      commands(pc) = PickUpV(pc, c)
      pc += 1
    }
  }

  object eat {
    def apply(): Unit = {
      commands(pc) = EatAll(pc, mouth)
      pc += 1
    }
    def apply(x: Int) = {
      commands(pc) = EatN(pc, x)
      pc += 1
    }
    def apply(c: Container) = {
      commands(pc) = EatV(pc, c, 1)
      pc += 1
    }
  }

  object clear {
    def apply(c: Container)= {
      commands(pc) = ClearV(pc, c)
      pc += 1
    }
  }

  def stop: Unit = {
    commands(pc) = Exit(pc)
  }

  def routine(label: String): Unit = {
    commands(pc) = RoutineS(pc, label)
    pc += 1
  }

  def jump(label: String): Unit = {
    commands(pc) = JumpS(pc, label)
    pc += 1
  }

  def saveLeftovers(d: Double): Unit = {
    commands(pc) = SaveLeftoversN(pc, d)
    pc += 1
  }

  // The line "good boy" must be at the end of every program.
  object good {
    def boy: Unit = {
      commands(pc) = End(pc)
      evaluate(commands.keys.toList.sorted.head)
    }
  }

  /*
  * Runtime evaluator that executes after calling "good boy"
  */
  private def evaluate(line: Int): Unit = {

    commands(line) match {
      case BarkS(_, s: String, explicitRepeat: Int, referencedRepeat: Container) =>
        if (referencedRepeat.getVal != 0) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            print(s)
          }
        } else {
          for (itr <- 1 to explicitRepeat)
            print(s)
        }
        evaluate(line + 1)

      case PromptUser(_, explicitRepeat: Int, referencedRepeat: Container) =>
        if (referencedRepeat.getVal != 0) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            mouth += scala.io.StdIn.readInt()
          }
        } else {
          for (itr <- 1 to explicitRepeat)
            mouth += scala.io.StdIn.readInt()
        }
        evaluate(line + 1)

      case PrintMouth(_, explicitRepeat: Int, referencedRepeat: Container) =>
        var result = new DecimalFormat("0.###").format(mouth)
        if (referencedRepeat.getVal != 0) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            print(result)
          }
        } else {
          for (itr <- 1 to explicitRepeat)
            print(result)
        }
        evaluate(line + 1)

      case ClearMouth(_, explicitRepeat: Int, referencedRepeat: Container) =>
        var result = new DecimalFormat("0.###").format(mouth)
        if (referencedRepeat.getVal != 0) {
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

      case FetchN(_, num: Double, explicitRepeat: Int, referencedRepeat: Container) =>
        if (referencedRepeat.getVal != 0) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            mouth += num
          }
        } else {
          for (itr <- 1 to explicitRepeat)
            mouth += num
        }
        evaluate(line + 1)

      case FetchV(_, c: Container, explicitRepeat: Int, referencedRepeat: Container) =>
        if (referencedRepeat.getVal != 0) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            mouth += c.getVal
          }
        } else {
          for (itr <- 1 to explicitRepeat)
            mouth += c.getVal
        }
        evaluate(line + 1)

      case DropV(_, c: Container, explicitRepeat: Int, referencedRepeat: Container) =>
        if (referencedRepeat.getVal != 0) {
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

      case PickUpV(_, c: Container, explicitRepeat: Int, referencedRepeat: Container) =>
        if (referencedRepeat.getVal != 0) {
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

      case EatAll(_, num: Double, explicitRepeat: Int, referencedRepeat: Container) =>
        if (referencedRepeat.getVal != 0) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            mouth = 0
          }
        } else {
          for (iter <- 1 to explicitRepeat) {
            mouth = 0
          }
        }
        evaluate(line + 1)

      case EatN(_, num: Double, explicitRepeat: Int, referencedRepeat: Container) =>
        if (referencedRepeat.getVal != 0) {
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

      case EatV(_, c: Container, explicitRepeat: Int, referencedRepeat: Container) =>
        if (referencedRepeat.getVal != 0) {
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

      case ClearV(_, c: Container, explicitRepeat: Int, referencedRepeat: Container) =>
        if (referencedRepeat.getVal != 0) {
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
        if (referencedRepeat.getVal != 0) {
          if (referencedRepeat.repeat() > 0) {
            System.exit(0)
          }
        } else {
          System.exit(0)
        }

      case RoutineS(_, s: String, referencedRepeat: Container) =>
        if (referencedRepeat.getVal != 0 && referencedRepeat.repeat() > 0) {
          labels(s) = line
        } else {
          labels(s) = line
        }
        evaluate(line + 1)

      case JumpS(_, s: String, explicitRepeat: Int, referencedRepeat: Container) =>
        if (referencedRepeat.getVal != 0) {
          if (referencedRepeat.repeat() > 0) {
            val newLine = labels(s)
            evaluate(newLine + 1)
          } else {
            evaluate(line + 1)
          }
        } else {
          if (explicitRepeat > 0) {
            val newLine = labels(s)
            evaluate(newLine + 1)
          } else {
            evaluate(line + 1)
          }
        }

      case SaveLeftoversN(_, num: Double, explicitRepeat: Int, referencedRepeat: Container) =>
        if (referencedRepeat.getVal != 0) {
          for (iter <- 1 to referencedRepeat.repeat()) {
            mouth %= num
          }
        } else {
          for (itr <- 1 to explicitRepeat) {
            mouth %= num
          }
        }
        evaluate(line + 1)

      case End(_) =>
      case _ =>
    }
  }
}
