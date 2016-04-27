package dog

/**
  *
  */
object Demo extends Dog {

  def main(args: Array[String]) {
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
    safe1 jump ("label")
    jump ("other label")
    label "return"
    bark ("You rolled a ")
    give

    good boy
  }

  // test case
  def basic: Unit = {
    fetch(5)
    drop (floor)
    fetch(10)
    drop (floor)
    pickup (floor)
    give
    bark ("\n")
    pickup (floor)
    give
  }
}