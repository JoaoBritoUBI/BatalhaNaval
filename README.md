**Battleship in Haskell**

**Developed by:** João Brito (m9984) and André Martins (m10157)

# Description
This project allows the user to **play a game of *Battleship* against the computer**. It was implemented in **Haskell** and runs on a CLI.

# Setup
**1.** Download and install **GHC/GHCI** from the official downloads page (https://www.haskell.org/downloads/);

**2.** Type "*ghci*" on a terminal window, followed by "*:l Main.hs*";

**3.** Type "*main*" to launch the game;

**4.** Follow the game's instructions and have fun!

# Code structure
* **Main.hs**: contains the function responsible for actually playing the game;
* **AuxFunctions.hs**: contains a plethora of pure and impure functions that are needed throughout the game;
* **GameMoves.hs**: contains the functions responsible for placing the player's and computer's ships, as well as, registering their attacking moves;
* **GamePrints.hs**: contains the functions that print the game's interface (messages, information screens, etc...);
* **Board.hs**: contains the type definitions and instance of ***Show*** for the type ***BoardF***;
* **Constants.hs**: contains the game's settings (board size, ship lengths, etc...)
* **GameState.hs**: contains some records for the state of the game, as well as, an instance of ***Show*** for the record ***GameState***.

# Implementation details
## The Board
The game makes use of a record where we have **offense and defense boards** for both the player and the computer. In the **defense boards**, each player has its ships and the markings corresponding to the attacking attempts from the opposing player. These markings are:

* **`** for a Ship;
* **x** for a Hit;
* **o** for a Miss; 
* **\*** for a Sunken Ship.

As for the **offense boards**, they comprise the guesses made by each player, obviously not knowing where the opposing ships are located.

For each board (represented by the ***Board*** data record), we have a function that maps each coordinate (***Coord***) to a position state (***PositionState***) and a list of lists with the coordinates of every ship that belongs to the board/player in question.

## The Players
Both players go through the process of distributing the ships in their respective boards. The computer does so automatically (either randomly or wisely - this behaviour can be configured with the boolean ***wiseComputer*** in ***Constants.hs***), while the player is asked to enter coordinates for every ship in the following format:

    startCoord;endCoord
    e.g. (0,0);(0,4) - for a ship of size 5

Once the ships are in place, the game starts with each player attacking in turns.

As a side note, at any point, the player can press ***q*** to leave the game immediately.

## The Game
The player will be able to see both boards (**defense** and **offense**) like in a traditional ***Battleship*** game:
    
    o--------------------------------------------------------o
    |                          PLAY                          |
    o--------------------------------------------------------o

	            Player's Offense Board

         0   1   2   3   4   5   6   7   8   9
	   +---+---+---+---+---+---+---+---+---+---+
	0  |   |   |   |   |   |   |   |   |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	1  |   |   |   |   |   |   |   |   |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	2  |   |   |   |   |   |   |   |   |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	3  |   |   |   |   |   |   |   |   |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	4  |   |   |   |   |   |   |   |   |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	5  |   |   |   |   |   |   |   |   |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	6  |   |   |   |   |   |   |   |   |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	7  |   |   |   |   |   |   |   |   |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	8  |   |   |   |   |   |   |   |   |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	9  |   |   |   |   |   |   |   |   |   |   |
	   +---+---+---+---+---+---+---+---+---+---+

	  	      Player's Defense Board

	     0   1   2   3   4   5   6   7   8   9
	   +---+---+---+---+---+---+---+---+---+---+
	0  |   |   |   |   |   |   |   | ` |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	1  |   | ` | ` | ` | ` | ` |   | ` |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	2  | ` |   | ` | ` | ` | ` |   |   |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	3  | ` |   | ` |   | ` | ` | ` | ` |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	4  | ` | ` | ` |   |   |   |   | ` |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	5  |   |   | ` |   | ` |   |   | ` |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	6  |   |   |   |   | ` |   |   | ` |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	7  |   | ` | ` |   | ` |   |   |   |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	8  |   |   |   |   |   |   |   |   |   |   |
	   +---+---+---+---+---+---+---+---+---+---+
	9  |   |   |   |   |   |   |   |   |   |   |
	   +---+---+---+---+---+---+---+---+---+---+

	(Player) Attack position >

From this point forward, both players will enter coordinates in the familiar format (i.e. (line,column)) and wait for the program to state the outcomes, like so:

    o--------------------------------------------------------o
    |                       ROUND INFO                       |
    o--------------------------------------------------------o
    | (Player) Coordinate (1,1) was a MISS!                  |
    | (Computer) Coordinate (4,7) was a HIT!                 |
    o--------------------------------------------------------o

Eventually, the game will come to an end with a message indicating the winner (or winners, in case of a tie). Then, the coordinates of every ship will be revealed:

	o--------------------------------------------------------o
	|                       GAME OVER                        |
	o--------------------------------------------------------o

	The player has won the game!

## The Verdict
In general, the code makes use of both generic and Haskell specific functionalities, like:

* List Comprehension;
* Curried Functions;
* Lambda Expressions;
* IO Actions;
* State Transformer;
* Type Synonyms;
* Recursion.

Additionally, we made sure that we **gave each function a purpose** and **divided the tasks into functions**, rather than having one really big function to do it all. This approach gave the code more readability and a "functional" look and feel.
