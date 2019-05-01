
# Based on: https://github.com/awni/backgammon
# Other sources:: (textbook see pg.367) http://incompleteideas.net/book/bookdraft2017nov5.pdf
# https://www.cs.cornell.edu/boom/2001sp/Tsinteris/gammon.htm

# converting board into 294 features based on....

# def extractFeatures(state):
#   game,player = state
#   features = []
#   for p in game.players:    # 2 (i think)
#     for col in game.grid: # 24 on grid
#     feats = [0.]*6    # 6 
  #   if len(col)>0 and col[0]==p:
  #     for i in xrange(len(col)):
    #     feats[min(i,5)] += 1
#     features += feats
#     features.append(float(len(game.barPieces[p]))/2.)
#     features.append(float(len(game.offPieces[p]))/game.numPieces[p])
#   if player == game.players[0]:
#     features += [1.,0.]
#   else:
#     features += [0.,1.]
#   return np.array(features).reshape(-1,1)


findFeat <- function(boardF, player){
    features = c()
for (p in 1:2){
    for (col in 1:24){
      feats = c(0,0,0,0,0,0)
      if (boardF[col] > 0 && p == 1){
        # player 1 has pieces
        for ( i in  1:boardF[col]){
          feats[min(1+i,6)] = feats[min(1+i,6)] + 1
        }
       features = c(features, feats)
      }
      else if (boardF[col] < 0 && p == 2){
        for ( i in  1:abs(boardF[col])){
          #python is 0 indexed!
          # not sure why first term is always zero
          feats[min(1+i,6)] = feats[min(1+i,6)] + 1  
        }
        features = c(features, feats)
      }
      else{
        features = c(features, c(0,0,0,0,0,0))
      }
    }
  # 25=white bar, 26=white off, 27=black off, 28=black bar
  # bar and off board
  if (p == 1){
  
  features = c(features, boardF[25]/2.0, boardF[26]/15)
  }
  if (p == 2){
  features = c(features, boardF[28]/2.0, boardF[27]/15)
  }
}
    # which player player
    
    if (player){
      features = c(features, 0, 1)
    }
    else {
      features = c(features, 1, 0)
    }

  return(features)
  
}

# now importing trained weights (trained in python) and doing feedforward



sigmoid = function(x) 1 / (1 + exp(-x))


#features = extractFeatures((game,game.opponent(self.player)))
#hiddenAct = 1/(1+np.exp(-(self.w1.dot(features)+self.b1)))
#v = 1/(1+np.exp(-(self.w2.dot(hiddenAct)+self.b2)))
#py_install(c('pickle'))
#Sys.setenv(RETICULATE_PYTHON = './env/bin/python')
#require(reticulate)
#use_virtualenv('./env', required = TRUE)

#source_python("wimport.py")
#w <<- read_file("weights0.bin")

#importing weights from csv and not from python because Shiny Server
#doesn't seem to have easily functioning python environment
#for reticulate package

w1.in <- read.csv("w1.csv", header = F)
b1.in <- read.csv("b1.csv", header = F)
w2.in <- read.csv("w2.csv", header = F)
b2.in <- read.csv("b2.csv", header = F)
win_prob <- function(boardF, player){
  
  # get features
  feat = array(findFeat(boardF, player),dim = c(294,1))
  # feat = array(feat, dim = c(294,1))
  #w = readBin('weights0.bin', what="double")
  w1 = as.matrix(w1.in)  # 50 by 294
  b1 = as.matrix(b1.in)  # 1  by 50
  
  w2 = as.matrix(w2.in)  # 50 by 1
  b2 = as.matrix(b2.in)  # 1 by 1
  
  # z = 50x1 + 50x294 * 294x1
  z = t(b1) + w1 %*% feat
  
  # h = 1 + 1x50 * 50x1
  h = b2 + t(w2) %*% sigmoid(z)
  
  # returns prob of winning given board configuration
  return(sigmoid(h))
  
}

# given board configuration and roll find best move

ai_move <- function(boardF, roll, player){
  
  if (playerB){
    moves=find.all.possible.moves(flip.board(boardF),roll)
    v = 0
    for (i in 1:length(moves)){
      p = win_prob(moves[[i]], player)
      if (p>v){
        v = p
        best = moves[[i]]
      }
    }
    return(flip.board(best))
  }
  else{
    moves = find.all.possible.moves(boardF, roll)
    v=0
    for (i in 1:length(moves)){
      p = win_prob(moves[[i]], player)
      if (p>v){
        v = p
        best = moves[[i]]
      }
    }
    return(best)
  }
  
}




