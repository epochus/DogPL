package dog
import scala.collection.mutable

/**
  *
  */
class Dog {
  abstract sealed class DogLine

  case class PrintString(num: Int, s: String) extends DogLine

  var pc = 1
  var mouth = 0
  var plate1 = 0

  var lines = new mutable.HashMap[Int, DogLine]

  object bark {
    def apply(str: String): Unit = {
      print(str)
    }
  }
}
