get.seed <- function(card){return(card & as.raw(0x30))}

get.card.strength <- function(card){return(card & as.raw(0x0f))}

is.max.player <-  function(card){return((card & as.raw(0xc0)) == as.raw(0x40))}

is.trump <- function(card, trump){return(get.seed(card) == trump)}

card.eval <- function(card){
  val <- get.card.strength(card)
  if (val < as.raw(0x07)){return(0)}
  if (val < as.raw(0x0a)){return (as.raw(as.integer(val) - 5)) }
  return (val)
}

is.captured <- function(card1, card2, trump){
  if (get.seed(card1) == get.seed(card2)){
    return (get.card.strength(card1) < get.card.strength(card2))
  }
  return (is.trump(card2, trump))
}

encoder <- function(deck){
  len <- length(deck)
  code.deck <- raw(len)
  for (i in seq_along(deck)){
    value <- gsub("\\D+","", deck[i]) # %>% print
    seed <- gsub("*\\d+", "",deck[i])
    val <- as.raw(as.integer(value) - 1)
    if (as.integer(value) == 1){val <- as.raw(11)}
    if (value == "3"){val <- as.raw(10)}
    
    if(seed == "golds") {sed <- as.raw(0)}
    if(seed == "trophies") {sed <- as.raw(32)}
    if(seed == "swords") {sed <- as.raw(16)}
    if(seed == "logs") {sed <- as.raw(48)}
    
    code.deck[i] <- sed | val
  }
  return(code.deck)
}

decoder <- function(vector.enc){
  len <- length(vector.enc)
  deck <- character(len)
  for (i in seq_along(vector.enc)){
    card <- vector.enc[i]
    seed <- as.integer(get.seed(card))
    value <- as.integer(get.card.strength(card))
    if (seed == 0){sed <- "golds"}
    if (seed == 32){sed <- "trophies"}
    if (seed == 48){sed <- "logs"}
    if (seed == 16){sed <- "swords"}
    
    val <- as.character(value + 1)
    if(value == 11){val <- "1"}
    if(value == 10){val <- "3"}
    
    cards <- paste0(sed, val)
    deck[i] <- cards
  }
  return(deck)
}
