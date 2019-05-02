source("nn-utils.R")
source("backgammon_board.R")
source("agents.R")

fwd.prop=function(board,weights,f=relu,g=sigmoid){
  # forward propegates current game state and returns 
  # each layer's activation. 
  # a2 is the network's output (aka percetentage chance for winning given board)
  z1 = weights$b1+weights$w1%*%as.matrix(board)
  a1 = f(z1)
  z2 = weights$b2+weights$w2%*%a1
  a2 = g(z2)
  return(list(a1=a1,z1=z1,a2=a2,z2=z2))
}

bk.prop=function(x,y,w2,fprop,df){
  # finds gradients of each layer & variable
  m = ncol(y)
  yhat = fprop$a2
  a1 = fprop$a1
  z1 = fprop$z1
  # L = 2
  dc2 = (yhat-y)
  db2 = rowSums(dc2)/m
  dw2 = (dc2 %*% t(a1))/m
  dc1 = df(z1)*(t(w2)%*%dc2)
  # L = 1
  db1 = rowSums(dc1)/m
  dw1 = (dc1 %*% t(x))/m
  return(list(db1=db1,db2=db2,dw1=dw1,dw2=dw2))
}

train.game=function(agent,ALPHA,LAMBDA,verbose){
  
  turns=0
  roll.dice=roll.dice.cl()
  board=flip.board(init.board())
  player=-1
  cost = 0
  reward_hist = list()
  
  while(game.over(board)==0){
    turns=turns+1
    # NEW TURN
    
    # Flips the board so that it's always from the white tile's perspective
    board=flip.board(board)

    player=-player
    roll=roll.dice()
    
    old.board=board
    board = agent$move(board, roll)
    
    #print(paste("Player",player))
    if (game.over(board)!=0){
      # Game is over! The current player won. 
      # Backpropegate with a reward of 1
      reward = 1
      output = fwd.prop(old.board,agent$weights,agent$f)
      actual = output$a2[1][1]
      #print(paste("Game over! Actual reward for winner:",actual))
      bprop_win = bk.prop(old.board,as.matrix(reward),agent$weights$w2,output,agent$df)
      # backprop with reward of 0
      output_loss = fwd.prop(flip.board(old.board),agent$weights,agent$f)
      bprop_loss = bk.prop(flip.board(old.board),as.matrix(0),agent$weights$w2,output_loss,agent$df)
      bprop = list()
      bprop$db1 = bprop_loss$db1 + bprop_win$db1
      bprop$db2 = bprop_loss$db2 + bprop_win$db2
      bprop$dw1 = bprop_loss$dw1 + bprop_win$dw1
      bprop$dw2 = bprop_loss$dw2 + bprop_win$dw2
      #print(paste("Actual for loser:",output_loss$a2[1][1]))
      win_val = actual
      lose_val = output_loss$a2[1][1]
      error = 1
      #print(win_val-lose_val)
    } else {
      # Backprop, using current evaluation as desired output,
      # and the board position previous to the current move as the input.
      reward = fwd.prop(board,agent$weights,agent$f)$a2
      output = fwd.prop(old.board,agent$weights,agent$f)
      actual = output$a2[1][1]
      #print(paste("Reward:",reward[1][1],"Actual:",actual))
      bprop = bk.prop(old.board,reward,agent$weights$w2,output,agent$df)
      reward = reward[1][1]
      error = reward - actual
    }
    
    
    reward_hist[[turns]] = reward
    # Update all gradient
    agent$ets$b1 = LAMBDA*agent$ets$b1 + bprop$db1
    agent$weights$b1 = agent$weights$b1 - ALPHA*agent$ets$b1*error
    #agent$weights$b1 = agent$weights$b1 - ALPHA*bprop$db1
    
    agent$ets$b2 = LAMBDA*agent$ets$b2 + bprop$db2
    agent$weights$b2 = agent$weights$b2 - ALPHA*agent$ets$b2*error
    #agent$weights$b2 = agent$weights$b2 - ALPHA*bprop$db2
    
    agent$ets$w1 = LAMBDA*agent$ets$w1 + bprop$dw1
    agent$weights$w1 = agent$weights$w1 - ALPHA*agent$ets$w1*error
    #agent$weights$w1 = agent$weights$w1 - ALPHA*bprop$dw1
    
    agent$ets$w2 = LAMBDA*agent$ets$w2 + bprop$dw2
    agent$weights$w2 = agent$weights$w2 - ALPHA*agent$ets$w2*error
    #agent$weights$w2 = agent$weights$w2 - ALPHA*bprop$dw2

    cost = cost + cost.squared.error(old.board,reward,agent$weights,g=sigmoid)
    
    play=describe.move(roll,old.board,board)
    #history[[length(history)+1]]=list(player=player,roll=roll,play=play,board=old.board)
    if(verbose)print.board(board)
    if(!check.board(board)!=0)stop("bad board")
  }
  if(player==-1)board=flip.board(board)
  #history[[length(history)+1]]=list(player=player,roll=NA,board=board)
  #print(paste("gave.over=",player,game.over(board)))
  #print.board(board)
  list(agent=agent,player=player,turns=turns,history=history,cost=cost,reward_hist=reward_hist,win_val=win_val,lose_val=lose_val)
}



nnet1.fit=function(agent,ALPHA,LAMBDA,max.games=500,EVAL){
  
  cost_hist = list()
  weight_hist = list()
  eval_hist = list()
  reward_hist = list()
  win_val_hist = list()
  loss_val_hist = list()
  
  for (i in 1:max.games){
    
    # trains for a full game
    game = train.game(agent,ALPHA,LAMBDA,FALSE)
    agent = game$agent
    weight_hist[[i]] = agent$weights
    cost_hist[[i]] = game$cost
    reward_hist[[i]] = game$reward_hist
    win_val_hist[[i]] = game$win_val
    loss_val_hist[[i]] = game$lose_val

    if(EVAL && (i%%100==0)){
      check = eval(agent,make_random_agent(),100)$p1
      print(paste(i/100,check))
      eval_hist[[(i/100)[1]]] = check
    }
    
  }
  return(list(agent=agent,weight_hist=weight_hist,cost=cost_hist,eval=eval_hist,reward_hist=reward_hist,wv_hist=win_val_hist,lv_hist=loss_val_hist))
}



