Dog Documentation
==================
In this language, imagine yourself as a person who can command a dog.
Numbers (Ints, Doubles) are represented as dog treats.
Dog treats can be stored in your dog's mouth, in **bowl**s, **safe**s, and the **floor**.
There are ten bowls and ten safes with 0...9 appended at the end. The floor can hold as many dog treats as you want.
When you get a dog treat from the floor, a random number of dog treats that isn't zero is chosen from the floor.

### Command List
Each command must be separated by a newline.
Any command can be preceded by a number or a variable (bowl, safe, or floor) which tells your dog how many times to execute that command.
If a command is not preceded by anything and does not take an argument, it does not need empty parentheses.
Replace **X** with either bowlX, safeX, floor, or a number.
Replace **V** with either bowlX, safeX, or floor.
Square brackets means the argument is optional.

**fetch (X)** : Put X treats into dog's mouth, adding on to whatever number is already there.

**drop (V)** : Move treats from dog's mouth into V, adding on to whatever number is already there. 

**pickup (V)** : Move treats from V to dog's mouth, adding on to whatever number is already there.

**eat ([X])** : Subtract X amount of treats from dog's mouth. If X is not provided, dog eats all the treats.

**clear (V)** : Resets V to 0. If V is floor, it resets the entire floor to 0.

**die** : Ends the program.

**take** : Waits for user input and adds that number to the dog's mouth.

**show** : Prints the number that is in the dog's mouth.

**give** : Prints the number that is in the dog's mouth and resets dog's mouth to 0.

**bark ("string")** : Prints whatever is in quotes to standard output. Use \n to print a newline. 

**jump ("label")** : Jumps to the routine "label".

**routine ("label")** : Defines where a dog can jump to when jumping to the routine "label".

### Features
------------------
- Standard Output
- User Input
- Mathematical Operations
- Conditional branching
- Loops
