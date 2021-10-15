# Mine Sweeper
Mine Sweeper with impossible right click feature

![screenshot](https://github.com/abapGames/minesweeper/blob/4a8752f5e9e980857eb070f7fc3951f8f6246a7f/img/SNAG-0094.png)

# todo
* minesweeper dows not notice when game is ended
* display number of unmarked mines
* display number of fields to reveal

# mode of operation
* build selection screen using macros
* before game start CLASS_CONSTRUCTOR is called to check if Dynpro 4000 already exists
  * if not: the selection-screen 1000 will be copied to 4000 with adaption to allow right-click for button
* call screen 9000 with embedded dynpro 4000 

