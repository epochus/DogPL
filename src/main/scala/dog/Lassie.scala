package dog

/**
  *
  */
object Lassie extends Dog {

  def main(args: Array[String]): Unit = {
    bark ("\n")
    6 bark("\t")
    bark (" ")
    26 bark ("_")
    bark ("\n")
    5 bark ("\t")
    bark ("    /")
    6 bark (" ")
    bark ("Lassie Save The")
    5 bark (" ")
    bark ("\\\n")
    5 bark ("\t")
    bark ("   |")
    13 bark (" ")
    bark ("Day!")
    11 bark (" ")
    bark ("|\n")
    5 bark ("\t")
    bark ("   |")
    28 bark (" ")
    bark ("|\n")
    5 bark ("\t")
    bark ("   |")
    bark ("  Enter your name to begin  |\n")
    5 bark ("\t")
    bark ("    \\")
    26 bark ("_")
    bark ("/\n\n")

    bark ("Enter your name: ")
    remember
    bark ("\n\nHello ")
    talk
    bark (",\n\n")
    bark ("You are lost in the dungeon with your trusty collie dog, Lassie.\n")
    bark ("You decide that your only hope of escape is for Lassie to go home and get a search party.\n")
    bark ("Go Lassie, save the day!\n\n")
    85 bark ("=")
    bark ("\n\n")
    forget
    bark ("Press Enter to continue...")
    remember
    bark("\n")
    forget

    fetch (3)
    drop (safe0)
    bark ("\t\t")
    routine ("Top")
      16 bark ("_")
      4 bark (" ")
      pickup (safe0)
      eat (1)
      drop (safe0)
    safe0 jump ("Top")

    fetch (3)
    drop (safe0)
    routine ("Filler1")
      bark ("\n\t   ")
      bark ("|")
      16 bark (" ")
      bark ("|")
      bark ("  ")
      bark ("|")
      16 bark (" ")
      bark ("|")
      bark ("  ")
      bark ("|")
      16 bark (" ")
      bark ("|")
      pickup (safe0)
      eat (1)
      drop (safe0)
    safe0 jump ("Filler1")

    fetch (1)
    drop (bowl0)
    fetch (1)
    drop (safe0)
    bark ("\n\t   ")

    memorize("|")
    7 memorize(" ")
    talk
    bark ("1")
    forget
    8 memorize(" ")
    memorize("|")
    memorize("  ")
    memorize("|")
    7 memorize(" ")
    talk
    bark ("2")
    talk
    bark ("3")
    forget
    8 memorize (" ")
    memorize ("|")
    talk
    forget

    fetch (1)
    drop (safe0)
    routine ("Filler2")
      bark ("\n\t   ")
      bark ("|")
      16 bark (" ")
      bark ("|")
      bark ("  ")
      bark ("|")
      16 bark (" ")
      bark ("|")
      bark ("  ")
      bark ("|")
      16 bark (" ")
      bark ("|")
      pickup (safe0)
      eat (1)
      drop (safe0)
    safe0 jump ("Filler2")

    fetch (1)
    drop (safe0)
    routine ("Filler3")
      bark ("\n\t   ")
      bark ("|")
      11 bark (" ")
      bark ("()")
      3 bark (" ")
      bark ("|")
      bark ("  ")
      bark ("|")
      11 bark (" ")
      bark ("()")
      3 bark (" ")
      bark ("|")
      bark ("  ")
      bark ("|")
      11 bark (" ")
      bark ("()")
      3 bark (" ")
      bark ("|")
      pickup (safe0)
      eat (1)
      drop (safe0)
    safe0 jump ("Filler3")

    fetch (3)
    drop (safe0)
    routine ("Filler4")
      bark ("\n\t   ")
      bark ("|")
      16 bark (" ")
      bark ("|")
      bark ("  ")
      bark ("|")
      16 bark (" ")
      bark ("|")
      bark ("  ")
      bark ("|")
      16 bark (" ")
      bark ("|")
      pickup (safe0)
      eat (1)
      drop (safe0)
    safe0 jump ("Filler4")

    eat
    bark ("\n\n")
    bark ("Lassie is confronted with three doors, each labeled with a number.\n")
    bark ("Which door should Lassie choose? ")
    take
    85 bark ("=")
    bark ("\n\n")
    eat (1)
    drop (safe1)
    safe1 jump ("Door 2 or 3")
    jump("Door 1")
    routine("Door 2 or 3")
    pickup (safe1)
    eat (1)
    drop (safe1)

    safe1 jump("Door 3")
    jump ("Door 2")

    routine ("Door 3")
    bark ("Lassie opens door 3 with an air of confidence. She looks forward and sees a house just a little farther " +
      "down the road.\n")
    bark ("She then looks downward and sees herself being engulfed in a room full of molten lava. The dog dies.\n")
    die
    bark("\nYOU LOSE\n")
    jump ("End")

    routine ("Door 2")
    bark ("Lassie opens door 2 with complacency. She encounters a giant granite wall blocking her path.\n")
    bark("It is too heavy to move, and she cannot move around it.\n")
    bark ("In the middle of the wall, Lassie sees a keypad and an engraving:\n")
    bark ("\n")
    eat
    25 bark (" ")
    bark ("%(*)!")
    25 bark (" ")
    bark ("\n")

    bark ("You have one guess: ")
    take
    3 bark ("\n")
    saveLeftovers(59801)
    drop (safe2)
    safe2 jump ("Wrong Number")
    jump ("Right Number")
    routine ("Wrong Number")
    bark ("The keypad makes a loud blaring noise and the wall stays still. Lassie is trapped inside. Forever.\n")
    bark ("\nYOU LOSE\n")
    jump ("End")
    routine ("Right Number")
    bark ("You enter the number \"59801\" on the keypad. The keypad makes a soft click and the wall breaks down.\n")
    bark ("Lassie sees a house to the distance and continues forth.")
    bark ("\n\nYOU WIN\n")
    jump ("End")

    routine("Door 1")
    eat
    fetch (1)
    drop (floor)
    fetch (2)
    drop (floor)
    fetch (3)
    drop (floor)
    fetch (4)
    drop (floor)

    bark ("Lassie opens door 1 vigorously. She encounters a fortune-teller blocking her way.\n")
    bark ("The fortune-teller will lead Lassie to the right direction if she answers this riddle correctly.\n\n")

    bark ("A princess is as old as the prince will be when the princess is twice as old as the\n")
    bark ("prince was when the princess' age was half the sum of their present age.\n")
    bark ("Which of the following could be true?\n")
    bark ("1. The prince is 30 and the princess is 40.\n")
    bark ("2. The prince is 40 and the princess is 30.\n")
    bark ("3. The prince is 20 and the princess is 30.\n")
    bark ("4. The prince is 30 and the princess is 20.\n")
    bark ("5. Take a random guess.\n\n")

    bark("What will Lassie choose? ")
    clear (safe0)
    clear (safe1)
    clear (safe3)
    clear (safe4)

    take

    drop (safe0)
    fetch (safe0)

    saveLeftovers(5)
    drop (safe4)

    safe4 jump ("Not random")
    jump ("random")
    routine ("Not random")
      pickup (safe0)
    jump ("Choice")
    routine ("random")
     eat
     fetch (floor)
    jump ("Choice")

    routine ("Choice")
      bark ("Lassie chose choice ")
      show
      bark (".\n")

      eat(1)
      drop (safe1)
      fetch (safe1)

      safe1 jump ("Wrong")
      jump ("Right")
      routine ("Wrong")
        bark ("The fortune-teller grimaces and suddenly causes the exit to collapse. ")
        bark ("Lassie is trapped inside. Forever.\n")
        bark ("\nYOU LOSE\n")
      jump ("End")

      routine ("Right")
        bark ("The fortune-teller smiles and suddenly disappears.\n")
        bark ("Lassie sees the way home and continues forth.\n")
        bark ("\nYOU WIN\n")
      jump ("End")
    jump ("End")

    routine("End")
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
}