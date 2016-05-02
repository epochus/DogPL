package dog

/**
  *
  */
object Demo extends Dog {

  def main(args: Array[String]): Unit = {
    bark("Please enter two numbers:\n")
    take()
    drop(bowl0)
    take()
    drop(bowl1)
    pickup(bowl0)
    show()
    drop(bowl0)
    bark(" times ")
    pickup(bowl1)
    show()
    drop(bowl1)
    bark(" equals ")
    bowl1 fetch (bowl0)
    show()
    drop(bowl2)
    fetch(50)
    eat(bowl2)
    drop(safe0)
    safe0 jump("smaller")
    jump("larger")
    routine("smaller")
    bark("\nWow, this number is smaller than 50!")
    jump ("end")
    routine("larger")
    bark("\nWow, this number is larger than 50!")
    routine("end")

    good boy
  }

  def conditionalBranching(): Unit = {
    bark("Please enter two numbers:\n")
    take()
    drop(bowl0)
    take()
    drop(bowl1)
    pickup(bowl0)
    show()
    drop(bowl0)
    bark(" times ")
    pickup(bowl1)
    show()
    drop(bowl1)
    bark(" equals ")
    bowl1 fetch (bowl0)
    show()
    drop(bowl2)
    fetch(50)
    eat(bowl2)
    drop(safe0)
    safe0 jump("smaller")
    jump("larger")
    routine("smaller")
    bark("\nWow, this number is smaller than 50!")
    jump ("end")
    routine("larger")
    bark("\nWow, this number is larger than 50!")
    routine("end")

    good boy
  }
/*
  // problem
  def test1: Unit = {
    fetch (3)
    drop (bowl1)
    fetch (5)
    give
    fetch (bowl1)
    bark("\n")
    give
  } //cannot fetch container

  
  def test2: Unit = {
    fetch (3)
    drop (bowl1)
    (bowl1) fetch (bowl1)
    give
    bark("\n")
    
  }


  def test3: Unit = {
    fetch (3)
    eat (2)
    give
    fetch (2)
    drop (bowl1)
    fetch (5)
    eat (bowl1)
    give
    fetch (4)
    eat
    give
  }

  // test1
  def dice: Unit = {
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

  }
*/
  

}