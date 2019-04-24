source("nn-utils.R")

### ALL AGENTS must have a move function defined. This will be called in the play.game.
### each move function should take as arguments 1. current board state, and 2. roll; they should 
    # return the new board state


### AGENT No. 1: vanilla agent! ###
make_vanilla_agent=function(n.hid,f,df,n.in=28){
  weights = init.wgts(n.in,n.hid,1)
  ets = init.zeros(n.in,n.hid,1)
  move=td.move
  return(list(weights=weights,ets=ets,move=move))
}

td.move=function(board,roll,vanilla_agent){
  moves=find.all.possible.moves(board,roll)
  max = -1
  new_move = moves[0]
  for (i in length(moves)){
    curr = fwd.prop(move[i],agent)
    if (curr > max){
      new_move = moves[i]
      max = curr
    }
  }
  return(new_move)
}

make_human_agent=function(){
  move=human.move
  return(list(move=move))
}


human.move=function(){
  # TODO 
}

make_random_agent=function(){
  move=random.agent.move
  return(list(move=move))
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