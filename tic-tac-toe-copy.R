

display <- function(state){
  se <- seq(1,length(state))
  
  for (i in 1:length(state)){
    if (!is.na(state[i])) { se[i] <- state[i]}
  }
  c1 <- paste(se[1:3], collapse = " | ")
  c2 <- paste(se[4:6], collapse = " | ")
  c3 <- paste(se[7:9], collapse = " | ")
  cat(paste('', c1,"\n---+---+---\n",c2, "\n---+---+---\n", c3, collapse = ""))
}


update <- function(state, who, pos){
  state[as.integer(pos)] <- who
  return(state)
}



prompt_user <- function(who, state){
  while(1){
    str <- paste("Where should ",who, " play: ")
    x <- readline(str)
    x <- as.integer(x)
    
    if(x %in% seq(1,9) && is.na(state[x])) {
      state[x] <<- who
      output <- c(who, x)  
      return(output)
      break
    }
    else if (is.na(x) | x > 9 | x <= 0) {cat("Please type an integer between 1 and 9")}
    else if (!is.na(state[x])) {cat("Please enter a valid move")}
    else cat("Please enter a valid move")
  }
}

check_winner <- function(state){
  triples <- list(
    c(1,2,3),
    c(4,5,6),
    c(7,8,9),
    c(1,4,7),
    c(2,5,8),
    c(3,6,9),
    c(1,5,9),
    c(3,5,7) )
  
  ma <- NULL
  for (i in 1:length(triples)){
    ma <- rbind(ma, state[triples[[i]]])
  }
  for (i in 1:nrow(ma)){
    if (length(ma[is.na(ma)]) != 0 && length(unique(ma[i,])) == 1 && unique(ma[i,]) %in% 'x') {
      print("x wins")
      display(state)
      break
    } else if (length(ma[is.na(ma)]) != 0 && length(unique(ma[i,])) == 1 && unique(ma[i,]) %in% "o") {
      print("o wins")
      display(state)
      break
    } else if (length(ma[is.na(ma)]) == 0){
      store_vec <- NULL
      for (j in 1:nrow(ma)){
        store_vec <- c(store_vec, length(unique(ma[j,]))) }
      
      if (1 %in% store_vec){ #means there is a winner 
        winner <- unique(ma[which(store_vec == 1),]) 
        print(paste(winner,"wins"))
        display(state)
        break
      } else print("no winners"); display(state)
      break
    }
    else next
  }
}


check_winner2 <- function(state){
  triples <- list(
    c(1,2,3),
    c(4,5,6),
    c(7,8,9),
    c(1,4,7),
    c(2,5,8),
    c(3,6,9),
    c(1,5,9),
    c(3,5,7) )
  
  ma <- NULL
  for (i in 1:length(triples)){
    #temp <- state[triples[[i]]]
    ma <- rbind(ma, state[triples[[i]]])
  }
  for (i in 1:nrow(ma)){
    if (length(ma[is.na(ma)]) != 0 && length(unique(ma[i,])) == 1 && unique(ma[i,]) %in% 'x') {
      return("x wins")
      #print("x wins")
      #display(state)
      break
    } else if (length(ma[is.na(ma)]) != 0 && length(unique(ma[i,])) == 1 && unique(ma[i,]) %in% "o") {
      #print("o wins")
      #display(state)
      return("o wins")
      break
    } else if (length(ma[is.na(ma)]) == 0){
      store_vec <- NULL
      for (j in 1:nrow(ma)){
        store_vec <- c(store_vec, length(unique(ma[j,]))) }
      
      if (1 %in% store_vec){ #means there is a winner 
        winner <- unique(ma[which(store_vec == 1),]) 
        #print(paste(winner,"wins"))
        return(paste(winner,"wins"))
        #display(state)
        break
      } else return("no winners")#print("no winners!")
      break #Edit
    }
    else next
  }
}

check_winner_computer <- function(state){
  triples <- list(
    c(1,2,3),
    c(4,5,6),
    c(7,8,9),
    c(1,4,7),
    c(2,5,8),
    c(3,6,9),
    c(1,5,9),
    c(3,5,7)
  )
  
  ma <- NULL
  for (i in 1:length(triples)){
    #temp <- state[triples[[i]]]
    ma <- rbind(ma, state[triples[[i]]])
  }
  for (i in 1:nrow(ma)){
    if (length(ma[is.na(ma)]) != 0 && length(unique(ma[i,])) == 1 && unique(ma[i,]) %in% "x") {
      print("x wins")
      #display(state)
      break
    } else if (length(ma[is.na(ma)]) != 0 && length(unique(ma[i,])) == 1 && unique(ma[i,]) %in% "o") {
      print("o wins")
      #display(state)
      break
    } else if (length(ma[is.na(ma)]) == 0){
      store_vec <- NULL
      for (j in 1:nrow(ma)){
        store_vec <- c(store_vec, length(unique(ma[j,]))) 
      }
      if (1 %in% store_vec){ #means there is a winner 
        winner <- unique(ma[which(store_vec == 1),]) 
        print(paste(winner,"wins"))
        #display(state)
        break
      } else print("no winners"); 
      #display(state)
      break
    }
    else next
  }
}


check_winner_computer2 <- function(state){
  triples <- list(
    c(1,2,3),
    c(4,5,6),
    c(7,8,9),
    c(1,4,7),
    c(2,5,8),
    c(3,6,9),
    c(1,5,9),
    c(3,5,7)
  )
  
  ma <- NULL
  for (i in 1:length(triples)){
    #temp <- state[triples[[i]]]
    ma <- rbind(ma, state[triples[[i]]])
  }
  for (i in 1:nrow(ma)){
    if (length(ma[is.na(ma)]) != 0 && length(unique(ma[i,])) == 1 && unique(ma[i,]) %in% 'x') {
      #print("x wins")
      #display(state)
      return("x wins")
      break
    } else if (length(ma[is.na(ma)]) != 0 && length(unique(ma[i,])) == 1 && unique(ma[i,]) %in% "o") {
      #print("o wins")
      #display(state)
      return("o wins")
      break
    } else if (length(ma[is.na(ma)]) == 0){
      store_vec <- NULL
      for (j in 1:nrow(ma)){
        store_vec <- c(store_vec, length(unique(ma[j,]))) }
      
      if (1 %in% store_vec){ #means there is a winner 
        winner <- unique(ma[which(store_vec == 1),]) 
        #print(paste(winner,"wins"))
        #display(state)
        return(paste(winner,"wins"))
        break
      } else return("no winners")
      break
    }
    else next
  }
}


computer_turn <- function(state, play_order){  #need further tuning to make it optimal!!!
  #play_order = 1 computer play first 
  #play_order = 2 computer play second
  #use p2 on inside computer part 
  triples <- list(  #I added this part!!!
    c(1,2,3),
    c(4,5,6),
    c(7,8,9),
    c(1,4,7),
    c(2,5,8),
    c(3,6,9),
    c(1,5,9),
    c(3,5,7)
  )
  
  na_cell_index <- which(is.na(state)) #get the index of NAs on vector "state"
  
  if(length(which(is.na(state))) == 1){
    pos <- na_cell_index   #select a position with NA
  }else {pos <- sample(na_cell_index, 1)}
  
  #if (play_order == 1 && length(state[which(state %in% c("x", "o"))]) %% 2 == 0){
  for (i in 1:length(triples)){
    vec <- state[triples[[i]]]
    if (play_order == 1 && length(state[which(state %in% c("x", "o"))]) %% 2 == 0 &&
        length(vec[which(vec == "x")]) == 2 && length(vec[which(is.na(vec))]) == 1){
      vec_pos <- which(is.na(vec))  #index of NA on vec
      pos_optimal <- triples[[i]][vec_pos] #get the optimal position need to update
      return(pos_optimal)
      break 
    } else if (play_order == 1 && length(state[which(state %in% c("x", "o"))]) %% 2 == 0 && 
               length(vec[which(vec == "o")]) == 2 && length(vec[which(is.na(vec))]) == 1){
      vec_pos <- which(is.na(vec))
      pos_block <- triples[[i]][vec_pos]
      return(pos_block)
      break
    } else if (play_order == 2 && length(state[which(state %in% c("x", "o"))]) %% 2 == 1 && 
               length(vec[which(vec == "o")]) == 2 && length(vec[which(is.na(vec))]) == 1){
      vec_pos <- which(is.na(vec)) 
      pos_optimal <- triples[[i]][vec_pos]
      return(pos_optimal)
      break
    }
    else if (play_order == 2 && length(state[which(state %in% c("x", "o"))]) %% 2 == 1 && 
             length(vec[which(vec == 'x')]) == 2 && length(vec[which(is.na(vec))]) == 1){
      vec_pos <- which(is.na(vec))
      pos_block <- triples[[i]][vec_pos]
      return(pos_block)
      break }
  }
  return(pos)
}


state <- rep(NA,9)
play <- function(){
  #prompt user 
  p1 <- readline("how many human player? 1 or 2:")
  p1 <- as.integer(p1)
  
  if (p1 == 2) { #2 human players
    state <- rep(NA, 9)
    s <- "" #sample(letters, 1)  #arbitrary choose a string 
    while(!s %in% c("o wins", "x wins","no winners") || is.null(s)){
      na_num <- state[!is.na(state)]
      if (length(na_num) %% 2 == 0){
        display(state)
        y = prompt_user("x", state)  #use output: output[1] = who, output[2] = pos
        state <- update(state, y[1],y[2])#output[1], output[2])  #update the vector "state"
        check_winner(state)
        s <- check_winner2(state)
      } else if (length(na_num) %% 2 == 1){
        display(state)
        y = prompt_user("o", state) 
        state <- update(state, y[1], y[2])#output[1], output[2])
        check_winner(state)
        s <- check_winner2(state)
      }
    }
  } else if (p1 == 1) { #computer part !
    p2 <- readline("Should the computer play first or second? 1 or 2:")
    p2 <- as.integer(p2)
    
    if (p2 == 1){  #computer plays first --
      state <- rep(NA, 9)
      s <- "i"
      while(!s %in% c("o wins", "x wins","no winners") || is.null(s)){
        na_num_c <- state[!is.na(state)]
        if (length(na_num_c) %% 2 == 0){ #computer turn 
          pos <- computer_turn(state, p2) 
          #update board
          state <- update(state, "x", pos)
          cat("x play",pos)
          cat("\n")
          check_winner(state) #------
          s <- check_winner_computer2(state)
          #cat("x play", pos)
          #display(state)
        }
        else if (length(na_num_c) %% 2 == 1){ #player turn
          display(state)
          y <- prompt_user("o", state)
          state <- update(state, y[1], y[2]) #output[1], output[2])
          #check_winner_computer(state)
          check_winner(state)
          #s <- check_winner_computer2(state)
          s <- check_winner2(state)
          display(state)
          cat("\n")  
        }
      }
    }else if (p2 == 2) { #computer play secondly 
      state <- rep(NA, 9)
      s <- "i"
      while(!s %in% c("o wins", "x wins","no winners") || is.null(s)){
        na_num_c <- state[!is.na(state)]
        if (length(na_num_c) %% 2 == 0){   #player's turn (1st)
          display(state)
          y <- prompt_user("x", state)
          state <- update(state, y[1],y[2])#output[1], output[2])
          check_winner_computer(state) 
          s <- check_winner2(state)
          cat("\n")
          display(state)  
          cat("\n")
        }
        else if (length(na_num_c) %% 2 == 1){ #computer's turn (2nd)
          pos <- computer_turn(state, p2)
          state <- update(state, "o", pos)
          cat("o play",pos)
          cat("\n")
          check_winner(state)  #check_winner_computer(state)
          s <- check_winner_computer2(state)
          #display(state)
        }
      }#while loop ends
    }
  }
}






