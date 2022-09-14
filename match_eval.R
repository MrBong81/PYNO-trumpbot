library(magrittr)
source("bytes_functions.R")

match_eval <- function(game, trump.card){
  eval <- 0
  v <- numeric(20)
  len <-  (game %>% length) %/% 2
  for (i in 1:len){
    add <- card.eval(game[2*(i - 1) + 1]) %>% as.integer + card.eval(game[2*(i - 1) + 2]) %>% as.integer
    
    if (i == 20){
      max <- is.max.player(game[39])
      captured <- is.captured(game[39], game[40], trump.card %>% get.seed)
      
      if (max == captured){add <- -add}
      eval <- eval + add
      
      return(eval)
      } # last round logic
    
    if (!(game[2*(i - 1) + 3] %>% is.max.player)){
      add <- -add
    }
    
    eval <- eval + add
    # add %>% print
    # eval %>% print
    
  }
  print(v)
}

# match_eval(h, as.raw(0x03) ) %>% print

