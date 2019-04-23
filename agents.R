### ALL AGENTS return the ***new*** board state. 

td.move=function(board,roll){
  moves=find.all.possible.moves(board,roll)
  # forward propegate each move
  # return new board state 
}

### 
human.move=function(){
  # TODO 
}

random.agent.move=function(board, roll){
  moves=find.all.possible.moves(board,roll)
  n=length(possible.moves)
  if (n>0) {
    i=sample(n,1)
    board=possible.moves[[i]]
  } else {
    if(verbose)print(paste(player,"unable to play"))
  }
  new.board=possible.moves[[i]]
  return(new.board)
}