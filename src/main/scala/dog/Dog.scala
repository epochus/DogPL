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
  var dish1 = 0
  var dish2 = 0
  var dish3 = 0

  var lines = new mutable.HashMap[Int, DogLine]

  object bark {
    def apply(str: String): Unit = {
      print(str)
    }
  }
}
