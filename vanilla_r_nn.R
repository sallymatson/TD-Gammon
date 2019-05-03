source("nn-utils.R")
source("backgammon_board.R")
source("agents.R")

fwd.prop=function(board,weights,f,g=sigmoid){
  # forward propegates current game state and returns 
  # each layer's activation. 
  # a2 is the network's output (aka percetentage chance for winning given board)
  z1 = weights$b1+weights$w1%*%as.matrix(board)
  a1 = f(z1)
  z2 = weights$b2+weights$w2%*%a1
  a2 = g(z2)
  return(list(a1=a1,z1=z1,a2=a2,z2=z2))
}

# x is the old board
# y is the fprop val from the next board
# fprop is forward propgating the old board
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
  p1_hist = list()
  p2_hist = list()
  p = "white"
  
  while(game.over(board)==0){
    turns=turns+1
    # NEW TURN
    
    # Flips the board so that the current player always sees it as a
    # white board (i.e. both players think they're white and move in the 
    # negative direction.)
    board=flip.board(board)
    
    player=-player
    roll=roll.dice()
    
    old.board=board
    
    # change the player
    if (p == "white") {
      p = "black"
      # It is the black player's turn. 
      # On black turns, the plan is to minimize the target function 
      # when passing in the white board.
      board = agent$move(board, roll, agent)
    } else {
      p = "white"
      board = agent$move(board, roll, agent)
    }
    
    if (game.over(board)!=0){ # GAME IS OVER
      
      if (p == "white") {
        # Game is over and white won.
        # Since the board is from white's perspective, 
        # backprop with a reward of 1.
        reward = 1
        pred_win = fwd.prop(old.board,agent$weights,agent$f)
        bprop = bk.prop(old.board,as.matrix(reward),agent$weights$w2,pred_win,agent$df)
        win_val = pred_win$a2[1][1]
        loss_val = fwd.prop(flip.board(old.board),agent$weights,agent$f)$a2[1][1]
        actual = win_val
      } else {
        # Game is over and black has won (i.e. white lost.)
        # Since it is black's turn, the board is currently from black's perspective. 
        # To backprop 0, we need to flip the board first to make it white's perspective.
        reward = 0
        pred_loss = fwd.prop(flip.board(old.board),agent$weights,agent$f)
        bprop = bk.prop(flip.board(old.board),as.matrix(reward),agent$weights$w2,pred_loss,agent$df)
        # compute a2 values for both people's current board view
        loss_val = pred_loss$a2[1][1]
        win_val = fwd.prop(old.board,agent$weights,agent$f)$a2[1][1]
        reward = 1
        actual = win_val
      }
    } else { # IF THE GAME IS NOT OVER
      
      if (p ==  "white"){
        # Backprop, using current evaluation as desired output,
        # and the board position previous to the current move as the input.
        # the board is from white's perspective, so we don't need to flip it.
        reward = fwd.prop(board,agent$weights,agent$f)$a2
        output = fwd.prop(old.board,agent$weights,agent$f)
        actual = output$a2[1][1]
        bprop = bk.prop(old.board,reward,agent$weights$w2,output,agent$df)
        reward = reward[1][1]
      } else {
        reward = fwd.prop(board,agent$weights,agent$f)$a2
        output = fwd.prop(old.board,agent$weights,agent$f)
        actual = output$a2[1][1]
        bprop = bk.prop(old.board,reward,agent$weights$w2,output,agent$df)
        reward = reward[1][1]
      }
    }
    
    error = abs(reward - actual)
    # Update all gradient
    agent$ets$b1 = LAMBDA*agent$ets$b1 + bprop$db1
    agent$weights$b1 = agent$weights$b1 - ALPHA*agent$ets$b1*error
    
    agent$ets$b2 = LAMBDA*agent$ets$b2 + bprop$db2
    agent$weights$b2 = agent$weights$b2 - ALPHA*agent$ets$b2*error
    
    agent$ets$w1 = LAMBDA*agent$ets$w1 + bprop$dw1
    agent$weights$w1 = agent$weights$w1 - ALPHA*agent$ets$w1*error
    
    agent$ets$w2 = LAMBDA*agent$ets$w2 + bprop$dw2
    agent$weights$w2 = agent$weights$w2 - ALPHA*agent$ets$w2*error
    
    cost = cost + cost.squared.error(old.board,reward,agent$weights,agent$f,g=sigmoid)
    
    if (p == "white"){
      p1_hist[[turns]] = actual
    } else {
      p2_hist[[turns]] = actual
    }
  }
  list(agent=agent,
       winner=p,
       w=player,
       turns=turns,
       p1_hist=p1_hist,
       p2_hist=p2_hist,
       cost=cost,
       win_val=win_val,
       loss_val=loss_val)
}



nnet1.fit=function(agent,ALPHA,LAMBDA,max.games=500,EVAL,eval_freq){
  
  cost_hist = list()
  weight_hist = list()
  eval_hist = list()
  reward_hist = list()
  win_val_hist = list()
  loss_val_hist = list()
  games_p1 = list()
  games_p2 = list()
  winner = list()
  
  for (i in 1:max.games){
    
    # trains for a full game
    game = train.game(agent,ALPHA,LAMBDA,FALSE)
    agent = game$agent
    weight_hist[[i]] = agent$weights
    cost_hist[[i]] = game$cost
    win_val_hist[[i]] = game$win_val
    loss_val_hist[[i]] = game$loss_val
    games_p1[[i]] = game$p1_hist
    games_p2[[i]] = game$p2_hist
    winner[[i]] = game$winner
    
    if(EVAL && (i%%eval_freq==0)){
      print(i)
      check = evaluate(agent,make_random_agent(),250)$p1
      print(paste(i/eval_freq,check))
      eval_hist[[(i/eval_freq)[1]]] = check
    }
  }
  return(list(agent=agent,weight_hist=weight_hist,cost=cost_hist,eval=eval_hist,reward_hist=reward_hist,wv_hist=win_val_hist,lv_hist=loss_val_hist,games_p1=games_p1,games_p2=games_p2,winner=winner))
}



