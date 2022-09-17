rm(list=ls())
library(magrittr)
source("DECKs.R")
source("match_eval.R")

####!!!!IMPORT IN THE ENVIROMENT THE DECK AND THE OTHER FUNCTIONS

### PLAYER ACTIONS
random.blind <- function(hand, trump){
  len <- length(hand)
  return(sample.int(len, size = 1))
} # RANDOM CHOICE FUNCTIONS

random.counter <- function(card, hand, trump){
  len <- length(hand)
  return(sample.int(len, size = 1))
}

pg.blind <- function(hand, trump){
  len <- length(hand)
  h <- decoder(hand)
  cat("\nYour hand : ",h, "\n")
  choice <- readline("Type the digit corresponding to the card:\n") %>% as.integer
  while(choice < 1 | choice > len){
    cat("!!!INVALID CHOICE!!!\n")
    cat("Your hand : ",h, " \n")
    choice <- readline("Type the digit corresponding to the card:\n") %>% as.integer
  }
  return(choice)
} # PLAYER FUNCTIONS

pg.counter <- function(card ,hand, trump){
  len <- length(hand)
  h <- decoder(hand)
  cat("\nCard played : ", decoder(card),"\nYour hand : ",h, " \n")
  choice <- readline("Type the digit corresponding to the card:\n") %>% as.integer
  while(choice < 1 | choice > len){
    cat("!!!INVALID CHOICE!!!\n")
    cat("Card played : ", decoder(card),"\nYour hand : ",h, " \n")
    choice <- readline("Type the digit corresponding to the card:\n") %>% as.integer
  }
  return(choice)
}


########## MATCH ############
MATCH_SIM <- function(blindp1,
                      counterp1,
                      blindp2,
                      counterp2,
                      deck) {
  set.seed(143)
  game <- sample(deck)
  histo <- raw(40)
  
  #GET THE TRUMP FROM LAST CARD OF THE DECK
  trump.card <- game[40]
  print(trump.card)
  
  #GET P1 AND P2 HANDS
  p1.hand <- game[1:3]
  p2.hand <- game[4:6]
  hist.idx <- 1
  deck.index <- 7
  round <- 1
  
  initiative <-  1 # or -1 i guess
  # ROUND 1 // X
  while (round <= 20) {
    cat("\n########### ROUND ", round, "##################\n","@@TRUMP CARD@@ :",trump.card %>% decoder)
    if (initiative == 1) {
      hand1 <- p1.hand
      blind <- blindp1
      hand2 <- p2.hand
      
      counter <- counterp2
    } else{
      hand1 <- p2.hand
      blind <- blindp2
      hand2 <- p1.hand
      
      counter <- counterp1
    }
    # cat ("Player 1 hand :" ,
    #      p1.hand,
    #      "\nPlayer 2 hand :",
    #      p2.hand,
    #      "\nPlayed cards are: ")
    #### PLAYING PHASE
    choice1 <- blind(hand1, trump.card)
    card1 <- hand1[choice1]
    choice2 <- counter(card1, hand2, trump.card)
    card2 <- hand2[choice2]
    # cat(card1, card2, "\n\n")
    #### RECORD FOR GAME OUTPUT
    adj1 <- as.raw(128)
    adj2 <- as.raw(64)
    if (initiative == 1) {
      adj1 <- as.raw(64)
      adj2 <- as.raw(128)
    }
    card1 <- card1 | adj1
    card2 <- card2 | adj2
    histo[hist.idx] <- card1
    hist.idx <- hist.idx + 1
    histo[hist.idx] <- card2
    hist.idx <- hist.idx + 1
    #### DEBUG PURPOSE
    
    cat("\nROUND PLAYED: ", decoder(c(card1, card2)),"\n\n")
    #### DROP CARDS OF THE HANDS
    if (initiative == 1) {
      p1.hand[choice1] <- as.raw(0x00)
      p2.hand[choice2] <- as.raw(0x00)
    } else {
      p1.hand[choice2] <- as.raw(0x00)
      p2.hand[choice1] <- as.raw(0x00)
    }
    
    #### SWITCH INITIATIVE
    if (is.captured(card1, card2, trump.card %>% get.seed)) {
      initiative <- initiative * -1
    }
    #### CARDS DRAW
    if ((hist.idx + 4) > 40) {
      p1.hand <- p1.hand[p1.hand != 0]
      p2.hand <- p2.hand[p2.hand != 0]
    }
    if (initiative == 1) {
      p1.hand[p1.hand == 0] <- game[deck.index]
      p2.hand[p2.hand == 0] <- game[deck.index + 1]
      deck.index <- deck.index + 2
    } else {
      p1.hand[p1.hand == 0] <- game[deck.index + 1]
      p2.hand[p2.hand == 0] <- game[deck.index]
      deck.index <- deck.index + 2
    }
    round <- round + 1
  }
  cat("HISTORY", histo,"\n")
  return(list(histo, trump.card))
}
################################

# h <- MATCH_SIM(p1.blindpick, p1.counter,p1.blindpick, p1.counter,deck %>% encoder)

###########Play a match VS a RANDOM GUESSING AI
h <- MATCH_SIM(pg.blind, pg.counter, random.blind, random.counter, deck %>% encoder)
cat("p1 score is :")
