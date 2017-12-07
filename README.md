# OVegas
![](https://i.imgur.com/kDWBsfF.jpg)
*A project brought to you courtesy of **Mishcat**, **Johanna**, **Ghassane**, and **Justin***


## TL;DR (assuming you have OCaml previously installed)

 1. Download the project files
 2. Navigate, using the `cd` command in your Command Line Interface (CLI), to the location of the project folder
 3. Type `make play` and hit enter
 4. Have fun!

## The Game

Everybody knows about Texas Hold'em. We love the game so we decided to bring it to the masses in the form of an amazing OCaml Project. Using it as a way to flex our newly earned Functional Programming skills (#MutabilityIsDead), OVegas will surely bring fun to both Poker veterans and newbies looking to form a gambling addiction!


### Installing OCaml

OCaml can take a bit to setup. So you might wanna grab your favorite edition of Cracking the Coding Interview and some Gimme! Coffee while you wait for it to get up and running. Fair warning!

So to set it all up, follow [this link right here](https://www.cs.cornell.edu/courses/cs3110/2017fa/install.html). This is the 'Installing OCaml for 3110' guide created by the amazing Professor Michael Clarkson and his 3110 ~~lackeys~~ staff. This is easily the most painless instruction manual for installing OCaml and getting it up and running on your machine. It also includes some quick setup instructions for the text editor Atom just in case you want to play around with the code or even start your own OCaml project!

> Justin swears by Visual Studio Code over Atom ("AND I'LL TAKE THAT TO MY GRAVE, THE INTEGRATED TERMINAL IS A LIFESAVER!")


### Starting The Game

Once OCaml has been installed and the Repo files downloaded (or in the case of the graders for the project, unzipping ovegassrc.zip):

 1. Navigate to the folder that contains the project files by using `cd` in your Terminal/Command Prompt
 2. Type `make play` and hit enter
 3. After it loads you are now in the game!

### Playing The Game
Texas Hold'em, as we discovered over these past few weeks, is an incredibly complex game with more rules than we could have imagined. So below are some "how the game works" type of mechanics as well as how to interact with our UI.

The game, after greeting you, will ask for your name so enter that.
![enter image description here](https://i.imgur.com/TpkxkB8.jpg)

It will then ask for a difficulty level, from which you can choose either Easy, Medium, or Hard

![enter image description here](https://i.imgur.com/TjE2J63.jpg)

And now the game has begun!

![enter image description here](https://i.imgur.com/gfAkvaF.jpg)

And now, courtesy of Mishcat, the extensive rules behind Texas Hold'em should you find yourself not understanding why a certain command is invalid.


----------


There are 4 betting rounds. Initially, there will be 0 shared cards in the center, and the AI will have 2 cards faced done. The players cards are shown to him. Each player has $500 initially.  The pot is $30, since the blinds have been taken for each player. The small blind is 10, and the big blind is 20. The valid commands in this pre-flop round are Raise, Call, Fold, and Quit. If the player calls, then you move into the flop betting round. If he raises, the other player can either raise or call that raise. If he calls, you move into the next round. If he raises, the same betting round keeps going until someone call, folds, or quits. If someone folds before showdown, the other player takes the pot, the play round is over, and the next play round begins with the blinds being taken again. In the next betting round (the flop betting round) the possible commands for the initial player are Bet, Check, Fold, and Quit. The commands available to the second player are Call, Fold, Raise, Quit, and Check (only if the first player also checked). This same exact logic applies to the next two betting rounds- the betting round after a turn card has been flipped, and the betting round after a river card has been flipped- also. Then the showdown round occurs if no player has folded up until then. In the showdown, the AI’s cards will be revealed, and the game logic determines who wins, and whoever wins, will get all the money in the pot. In the case of a tie, the pot will be split equally between the player and the AI. If either player has less than $20 then they lose as they would not be able to play big blinds in the next round if it is asked of them. Otherwise, if both players still have at least $20, then they go into the next play round, and the blinds are taken again, and the above rules still apply!
