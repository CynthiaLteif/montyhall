#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Select a door
#'
#' @description
#'   The select_door() function randomly selects one door from the three doors. 
#'
#' @details
#'   The function uses sample() function from R to randomly select one door from the three doors (1, 2, 3) and returns the selected door as a number between 1 and 3. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a number between 1 and 3 representing the selected door.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open goat door.
#'
#' @description
#'   open_goat_door function opens a door with a goat behind it.
#'
#' @details
#'   The function simulates the game and returns the door number of the door that is opened based on the player's selection and the door with the car behind it.
#'
#' @param game A vector of length 3 representing the contents behind the doors (either "car" or "goat").
#'
#' @param a.pick A single integer between 1 and 3 representing the player's initial door selection.
#' 
#' @return A single integer between 1 and 3 representing the door number that is opened.
#'
#' @examples
#'   open_goat_door(game, 1)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Change door.
#'
#' @description
#'   change_door simulates the player's decision to change or stay with the initial door selection
#'
#' @details
#'   The function returns the final door selection of the player based on whether they choose to stay with the initial selection or switch to the other door.
#'
#' @param stay A logical value indicating whether the player decides to stay with the initial door selection (TRUE) or switch to the other door (FALSE).
#' 
#' @param stay A logical value indicating whether the player decides to stay with the initial door selection (TRUE) or switch to the other door (FALSE).
#' 
#' @param opened.door A single integer between 1 and 3 representing the door number that was opened.
#' 
#' @param a.pick A single integer between 1 and 3 representing the player's initial door selection.
#' 
#' @return A single integer between 1 and 3 representing the final door selection of the player.
#'
#' @examples
#'   change_door(FALSE, 2, 1) # returns 3
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title Determine the Winner
#' 
#' @description
#'   determine_winner function checks the final selection and returns if the player wins or loses.
#' 
#' @details
#'   The function determines the winner of the game by checking the final selection of the player against the contents of the doors. 
#' 
#' @param final.pick A single integer between 1 and 3 representing the player's final door selection.
#' 
#' @param game A vector of length 3 representing the contents behind the doors (either "car" or "goat").
#' 
#' @return A character string of either "WIN" or "LOSE".
#' 
#' @examples
#'   determine_winner()
#' 
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title Play the Monty Hall Game
#'
#' @description
#' The function simulates the Monty Hall game and returns the outcome of the game for both the "stay" and "switch" strategies.
#'
#' @details
#' The function generates a new game, selects a door, opens a goat door, and calculates the final outcome for both "stay" and "switch" strategies.
#'
#' @param None
#'
#' @return A data frame with two rows and two columns. The first column is the strategy used, either "stay" or "switch", and the second column is the outcome, either "WIN" or "LOSE".
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function( )
{
new.game <- create_game()
first.pick <- select_door()
opened.door <- open_goat_door( new.game, first.pick )

final.pick.stay <- change_door( stay=T, opened.door, first.pick )
final.pick.switch <- change_door( stay=F, opened.door, first.pick )

outcome.stay <- determine_winner( final.pick.stay, new.game )
outcome.switch <- determine_winner( final.pick.switch, new.game )

strategy <- c("stay","switch")
outcome <- c(outcome.stay,outcome.switch)
game.results <- data.frame( strategy, outcome,
stringsAsFactors=F )
return( game.results )
}






#' @title Play n Monty Hall games
#'
#' @description Play n Monty Hall games and summarize the results.
#'
#' @details The function plays the Monty Hall game 'n' times, records the outcome of each game, and returns a table summarizing the proportion of wins and losses for each strategy (i.e., staying with the initial door selection or switching to another door).
#'
#' @param n An integer representing the number of games to play. Default is 100.
#'
#' @return A data frame containing the outcome of each game.
#'
#' @examples
#' play_n_games(n = 1000)
#'
#' @export
play_n_games <- function( n=100 )
{

library( dplyr )
results.list <- list() # collector
loop.count <- 1

for( i in 1:n ) # iterator
{
game.outcome <- play_game()
results.list[[ loop.count ]] <- game.outcome
loop.count <- loop.count + 1
}

results.df <- dplyr::bind_rows( results.list )

table( results.df ) %>%
prop.table( margin=1 ) %>% # row proportions
round( 2 ) %>%
print()

return( results.df )

}