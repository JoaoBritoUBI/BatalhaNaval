**Battleship in Haskell**

**Developed by:** João Brito (m9984) and André Martins (m10157)

# Description
This project allows the user to **play a game of *Battleship* against the computer**. It was implemented in **Haskell** and runs on a CLI.

# Setup
**1.** Download and install **GHC/GHCI** from the official downloads page (https://www.haskell.org/downloads/);

**2.** Type ***ghci*** on a terminal window, followed by ***:l Main.hs***;

**3.** Type ***main*** to launch the game;

**4.** Follow the game's instructions and have fun!

# Code structure
* **Main.hs**: contains the function responsible for actually playing the game;
* **AuxFunctions.hs**: contains a plethora of pure and impure functions that are needed throughout the game;
* **GameMoves.hs**: contains the functions responsible for placing the player's and computer's ships, as well as, registering their attacking moves;
* **GamePrints.hs**: contains the functions that print the game's interface (messages, information screens, etc...);
* **Board.hs**: contains the type definitions and instance of *Show* for the type *BoardF*;
* **Constants.hs**: contains the game's settings (board size, ship lengths, etc...)
* **GameState.hs**: contains some records for the state of the game, as well as, an instance of *Show* for the record *GameState*.

# Implementation details


# TO DO
* Solve the 2 proofs
* Implement a smarter computer
* Write a thorough README.md