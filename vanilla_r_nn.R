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
  game_hist = list()
  player = "black"
  
  while(game.over(board)==0 & game.over(flip.board(board))==0){

    turns=turns+1
    roll=roll.dice()
    old.board=board
    
    # change the player
    if (player == "white") {
      player = "black"
      board = agent$black.move(board, roll, agent)
    } else {
      player = "white"
      board = agent$move(board, roll, agent)
    }
    
    b_board = flip.board(board)
    if (game.over(board)!=0 | game.over(b_board)!=0){ # GAME IS OVER
      if (player == "white") {
        y = 1
      } else {
        y = 0
      }
    } else { # IF THE GAME IS NOT OVER
      y = fwd.prop(board,agent$weights,agent$f)$a2[1][1]
    }
  
    fprop = fwd.prop(old.board,agent$weights,agent$f)
    bprop = bk.prop(old.board,as.matrix(y),agent$weights$w2,fprop,agent$df)
    y_hat = fprop$a2[1][1]
    
    error = abs(y - y_hat)
    
    # Update all gradient
    agent$ets$b1 = LAMBDA*agent$ets$b1 + bprop$db1
    agent$weights$b1 = agent$weights$b1 - ALPHA*agent$ets$b1*error
    
    agent$ets$b2 = LAMBDA*agent$ets$b2 + bprop$db2
    agent$weights$b2 = agent$weights$b2 - ALPHA*agent$ets$b2*error
    
    agent$ets$w1 = LAMBDA*agent$ets$w1 + bprop$dw1
    agent$weights$w1 = agent$weights$w1 - ALPHA*agent$ets$w1*error
    
    agent$ets$w2 = LAMBDA*agent$ets$w2 + bprop$dw2
    agent$weights$w2 = agent$weights$w2 - ALPHA*agent$ets$w2*error
    
    # update history things
    cost = cost + cost.squared.error(old.board,y_hat,agent$weights,agent$f,g=sigmoid)
    game_hist[[turns]] = y
  }
  winner = y
  list(agent=agent,
       turns=turns,
       game_hist=game_hist,
       cost=cost,
       winner=winner)
}

nnet1.fit=function(agent,ALPHA,LAMBDA,max.games=500,EVAL,eval_freq){
  
  start.time=Sys.time()
  cost_hist = list()
  weight_hist = list()
  eval_hist = list()
  game_hist = list()
  winner_hist = list()
  
  for (i in 1:max.games){
    
    # trains for a full game
    game = train.game(agent,ALPHA,LAMBDA,FALSE)
    agent = game$agent
    weight_hist[[i]] = agent$weights
    cost_hist[[i]] = game$cost
    game_hist[[i]] = game$game_hist
    winner_hist[[i]] = game$winner
    
    if(EVAL && (i%%eval_freq==0)){
      print(i)
      check = evaluate(agent,make_random_agent(),500)$p1
      print(paste(i/eval_freq,check))
      eval_hist[[(i/eval_freq)[1]]] = check
    }
  }
  end.time=Sys.time()
  total.time = end.time - start.time
  return(list(agent=agent,cost=cost_hist,game_hist=game_hist,winner_hist=winner_hist,eval_hist=eval_hist,time=total.time))
}



