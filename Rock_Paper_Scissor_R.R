rock_paper_scissors <- function() {
  com <- c("Rock", "Paper", "Scissors")
  w <- 0
  l <- 0
  
  while (TRUE) {
    ans <- tolower(readline("Rock, Paper, Scissors, or Quit: "))
    comans <- sample(com, 1)
    
    if (ans == "quit") {
      break
    } else if (ans %in% c("rock", "paper", "scissors")) {
      cat("Computer chose:", comans, "\n")
      
      if (ans == comans) {
        cat("That's a tie!\n")
      } else if (
        (ans == "rock" && comans == "Scissors") ||
        (ans == "paper" && comans == "Rock") ||
        (ans == "scissors" && comans == "Paper")
      ) {
        cat("You win!\n")
        w <- w + 1
      } else {
        cat("You lose!\n")
        l <- l + 1
      }
    } else {
      cat("Invalid input. Please choose Rock, Paper, Scissors, or Quit.\n")
    }
  }
  
  cat("Wins:", w, "Losses:", l, "\n")
}

rock_paper_scissors()
