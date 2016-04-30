import dog.Dog
import org.scalatest.FlatSpec

/**
  *
  */
class TestSuite extends FlatSpec {


  "Rolling a dice" should "print a number between 1-6 inclusive" in {
    object RollingDice extends Dog {
      def run() = {
        fetch (1)
        drop (floor)
        fetch (2)
        drop (floor)
        fetch (3)
        drop (floor)
        fetch (4)
        drop (floor)
        fetch (5)
        drop (floor)
        fetch (6)
        drop (floor)
        pickup (floor)
        bark ("You rolled a ")
        give

        good boy
      }
    }
    RollingDice.run()
  }


}
