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

  "Fibonacci" should "" in {
    object Fibonacci extends Dog {
      def run(): Unit = {
        // This is how many numbers will be shown (has to be at least 4, otherwise it'll be an infinite loop
        fetch (10)

        // Subtract 3 from the above number before storing it, because of the initial two numbers and ending number
        eat (3)
        drop (safe1)

        // Store the initial two numbers (1 and 1) into dish1 and dish2, and also display them
        fetch (1)
        show
        bark (", ")
        drop (bowl1)
        fetch (1)
        show
        bark (", ")
        drop (bowl2)
        routine ("loop")
        // Show the next number
        fetch (bowl1)
        fetch (bowl2)
        show
        bark (", ")

        // Temporarily store result in dish3
        clear (bowl3)
        drop (bowl3)

        // Store whatever's in dish2 into dish1, and store the result into dish2
        clear (bowl1)
        pickup (bowl2)
        drop (bowl1)
        pickup (bowl3)
        drop (bowl2)

        // Decrement plate1
        pickup (safe1)
        eat (1)
        drop (safe1)
        safe1 jump ("loop")

        // Show the last number outside the loop (otherwise a comma would be printed after it)
        fetch (bowl1)
        fetch (bowl2)
        show

        good boy
      }
    }
    Fibonacci.run()
  }

  "99 Bottles of beer" should "" in {
    object beer extends Dog {
      def run(): Unit = {
        fetch (99)
        drop (safe1)
        routine ("loop")
        pickup (safe1)
        show
        bark (" bottles of beer on the wall, ")
        show
        bark (" bottles of beer.\n")
        bark ("Take one down and pass it around, ")
        eat (1)
        show
        bark (" bottles of beer on the wall.\n\n")
        drop (safe1)
        safe1 jump ("loop")
        bark ("No more bottles of beer on the wall, no more bottles of beer.\n")
        bark ("Go to the store and buy some more, 99 bottles of beer on the wall.")

        good boy
      }
    }
    beer.run()
  }

  "Fido" should " remember" in {
    object Fido extends Dog {
      def run(): Unit = {
        fetch(2)
        drop(bowl0)
        bowl0 memorize("Remember ")
        memorize("the fifth of November!")
        talk()
        forget()
        bark("\nRemember, remember the ")
        memorize("fifth")
        count()
        show()
        forget()
        memorize(" of November")
        talk()

        good boy
      }
    }
    Fido.run()
  }

  "Multiply" should "multiply" in {
    object Multiply extends Dog {
      def run(): Unit = {
        bark("This dog can multiply!\n")
        fetch(12)
        drop(bowl0)
        fetch(12)
        drop(bowl1)
        pickup(bowl0)
        show()
        drop(bowl0)
        bark(" times ")
        pickup(bowl1)
        show()
        drop(bowl1)
        bark(" equals ")
        bowl1 fetch bowl0
        give

        good boy
      }
    }
    Multiply.run()
  }

  "Division" should "divide" in {
    object Division extends Dog {
      def run(): Unit = {
        fetch(20)
        drop(safe0)
        fetch(5)
        drop(bowl1)
        routine("division")
          pickup(bowl0)
          fetch(1)
          drop(bowl0)
          pickup(safe0)
          eat(bowl1)
          drop(safe0)
        safe0 jump ("division")
        pickup(bowl0)
        give()

        good boy
      }
    }
    Division.run()
  }
}
