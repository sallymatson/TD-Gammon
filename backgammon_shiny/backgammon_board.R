


if(FALSE){
  library(rstudioapi)
  here=dirname(getActiveDocumentContext()$path)
  setwd(here)
  source("./board.R")
}

library(zeallot)

roll.dice.cl=function(seed){
  if(!missing(seed)) set.seed(seed)
  function(){
    roll=sample.int(6L,2,replace=TRUE)
    if(roll[1]==roll[2]) roll=c(roll,roll)
    sort(roll)
  }
}
roll.dice=roll.dice.cl()

# white moves in negative direction - all moves will be from white perspective
# executing flip.board will then reconfigure so black move looks the same as a white move
# points 1:24, 25=white bar, 26=white off, 27=black off, 28=black bar
# must set b.off and b.bar counts negative!
# GLOBAL VALUES! DO NOT CHANGE
w.bar=25
w.off=26
b.off=27
b.bar=28
points=1:24

init.white=function(){
  counts=integer(28)
  counts[c(6,8,13,24)]=c(5,3,5,2)
  counts
}
# this could be flip.board(init.white())
init.black=function(){
  counts=integer(28)
  counts[c(1,12,17,19)]=-c(2,5,3,5)
  counts
}

# check totals
check.board=function(board){
  sum(board)==0
}
# check this after making a move (moves are always white moves)
game.over=function(board){
  as.integer(board[w.off]==15)
}
gammon=function(board){
  as.integer(board[w.off]==15 && board[b.off]==0)
}
back.gammon=function(board){
  as.integer(gammon(board)==1 && (board[b.bar]!=0 || min(which(board[points]))<=6))
}
init.board=function(){init.white()+init.black()}
bar=function(board){board[w.bar]}
blot=function(board,pos){board[pos]==-1}
allowed.move=function(board,pos){board[pos]>-2}
print.board=function(board){
  print(paste0(paste(sprintf("%3d",board[1:24]),collapse=","),"|",
               paste(sprintf("%3d",board[25:28]),collapse=",")))
}
can.bear.off=function(board){
  board[w.bar]==0 & all(board[7:24]<=0)
}
flip.board=function(board){
  -c(rev(board[points]),rev(board[25:28]))
}
white.points=function(board){
  which(board[points]>0)
}
#-------GLOBAL-----
#possible.moves=NULL   # keep globally so not repeatedly copied
#game.history=NULL
verbose=FALSE
#------------------

# this code is no longer used
#append.board=function(new.board){
#  for(i in seq_along(possible.moves)){
#    if(all(new.board==possible.moves[[i]])) {
#print("duplicate board")
#      return(FALSE)
#    }
#  }
#  possible.moves[[length(possible.moves)+1]] <<- new.board
#  return(TRUE)
#}



play.game=function(verbose){
  history=list()
  turns=0
  roll.dice=roll.dice.cl()
  board=flip.board(init.board())
  player=-1
  while(game.over(board)==0){
    board=flip.board(board)
    turns=turns+1
    player=-player
    roll=roll.dice()
    
    if(verbose)print(paste("player=",player))
    if(verbose)print(paste("roll=",paste(roll,collapse=",")))
    if(verbose)print.board(board)
    moves=find.all.possible.moves(board,roll)
    #print(str(moves))
    if(!all(sapply(moves,is.vector))) stop
    if(verbose)print(paste("found",length(moves),"possible moves"))
    #if(length(moves)>0) for(i in seq_along(moves)) print.board(moves[[i]])
    #if(length(moves)>40)stop()
    if(FALSE){
      possible.moves <<- list()
      for(i in seq_along(moves)){
        append.board(moves[[i]])
      }
    }
    possible.moves <<- unique(moves)
    n=length(possible.moves)
    if(verbose)print(paste("found",n,"unique possible moves"))
    old.board=board
    i=0
    if(n>0){
      i=sample(n,1)
      board=possible.moves[[i]]
      #if(verbose)print.board(board)
    }else{
      if(verbose)print(paste(player,"unable to play"))
    }
    play=describe.move(roll,old.board,board)
    history[[length(history)+1]]=list(player=player,roll=roll,play=play,board=old.board)
    #print(is.list(board))
    #print(str(board))
    #print(length(board))
    #print(board)
    if(verbose)print.board(board)
    if(!check.board(board)!=0)stop("bad board")
  }
  if(player==-1)board=flip.board(board)
  history[[length(history)+1]]=list(player=player,roll=NA,board=board)
  #print(paste("gave.over=",player,game.over(board)))
  #print.board(board)
  list(player=player,turns=turns,history=history)
}
# returns a list of board configurations that can result from a roll
# note that the order of the dice can make a difference, so both orders are used
# assumes roll has 2 different integers or 4 identical integers
# WATCH OUT - uses global verbose setting
find.all.possible.moves=function(board,roll){
  one.move.boards=vector("list",2)
  two.move.boards=list()
  #print("start board")
  #print.board(board)
  if(roll[1]!=roll[2]){
    for(i in 1L:2L){
      one.move.boards[[i]]=make.single.move(board,roll[i])
      if(length(one.move.boards[[i]])>0){
        for(j in seq_along(one.move.boards[[i]])){
          two.move.boards=c(two.move.boards,make.single.move(one.move.boards[[i]][[j]],roll[3L-i]))
        }
        if(verbose) print(paste("i=",i,length(unique(two.move.boards))))
      }
    }
    if(length(two.move.boards)>0){
      return(two.move.boards)
    }
    if(verbose)print("returning one.move.boards")
    return(c(one.move.boards[[1]],one.move.boards[[2]]))
  }else{
    start.boards=list(board)
    end.boards=vector("list",4)
    for(i in 1:4){
      #print(paste("length(start.boards)=",length(start.boards)))
      for(j in seq_along(start.boards)){
        end.boards[[i]]=c(end.boards[[i]],make.single.move(start.boards[[j]],roll[i]))
      }
      if(is.null(end.boards[[i]]) || length(end.boards[[i]])==0) break;
      end.boards[[i]]=unique(end.boards[[i]])
      start.boards=end.boards[[i]]
    }
    if(i>1){
      return(end.boards[[i]])
    }
    return(list())
  }
}



# returns list of new board configurations that can result from moving according to just one die
# WATCH OUT - uses global verbose setting
make.single.move=function(board,roll){
  boards=list()
  # if bar is not empty, must re-enter all before moving board pieces
  if(board[w.bar]>0){
    if(verbose)print("bar move")
    q=w.bar-roll
    if(board[q]>-2) {
      new.board=board
      new.board[w.bar]=new.board[w.bar]-1
      if(new.board[q]==-1) {
        new.board[q]=0
        new.board[b.bar]=new.board[b.bar]-1
      }
      new.board[q]=new.board[q]+1
      boards[[length(boards)+1]]=new.board
    }
  }else{
    wp=white.points(board)
    if(!can.bear.off(board)) {  # if all points are not <= 6, then can't bearoff yet
      for(p in wp) {
        if(p > roll) {
          q=p-roll
          if(board[q]>-2){
            new.board=board
            
            #print(paste("move",p,"to",q))
            #print.board(new.board)
            new.board[p]=new.board[p]-1
            if(new.board[q]==-1) {
              new.board[q]=0
              new.board[b.bar]=new.board[b.bar]-1
            }
            new.board[q]=new.board[q]+1
            boards[[length(boards)+1]]=new.board
            #print.board(new.board)
          }
        }
      }
    }else{
      if(verbose)print("can bear off")
      for(p in wp) {
        q=p-roll
        if(q>=1){  # on-board move
          if(board[q]>-2){
            new.board=board
            new.board[p]=new.board[p]-1
            if(new.board[q]==-1) {
              new.board[q]=0
              new.board[b.bar]=new.board[b.bar]-1
            }
            new.board[q]=new.board[q]+1
            boards[[length(boards)+1]]=new.board
          }
        }else{
          if(verbose)print("bear off")
          new.board=board
          new.board[p]=new.board[p]-1
          new.board[w.off]=new.board[w.off]+1
          boards[[length(boards)+1]]=new.board
        }
      }
    }
  }
  boards
}

# to help the humans: given before/after board configurations, produce
# human readable description of the implied move
# will fail if the two board configurations are not connected by a single roll of the dice
describe.move=function(roll,before.board,after.board){
  require(gtools)
  diff=after.board-before.board
  #print(diff)
  blots=c()
  if(diff[b.bar] != 0){  # move hit 1 or more blots
    blots=which(before.board == -1 & after.board >= 0)
    #print(paste("blots=",paste(blots,collapse=",")))
    for(b in blots) before.board[b]=0 # was -1
    after.board[b.bar]=before.board[b.bar]
    diff=after.board-before.board
  }
  # diff can be >1
  from=c(which(diff<0),which(diff< -1),which(diff< -2),which(diff< -3))
  to=c(which(diff>0),which(diff>1),which(diff>2),which(diff>3))
  if(length(from)==0){
    return("no move")
  }
  if(length(roll)==2){
    roll=c(roll,sum(roll))
  }else{
    roll=c(roll,2*roll[1],3*roll[1],4*roll[1])
  }
  if(any(to==w.off)) roll=c(roll,from-w.off)  # append fake roll(s) to allow bearing off
  if(length(from)<=2){
    d=from-to
    if(!all(d %in% roll)) to=rev(to)
    if(!all((from-to) %in% roll)) {
      print(paste("roll=",paste(roll)))
      print.board(before.board)
      print.board(after.board)
      print(from)
      print(to)
      print(paste("diff=",paste(from-to,collapse=",")))
      stop("should give roll values")
    }
  }else{
    # will need to look at permutations
    n=length(from)
    map=permutations(n,n,1:n)
    for(row in 1:nrow(map)){
      p=map[row,]
      d=from-to[p]
      if(all(d %in% roll)){
        to=to[p];
        break;
      }
    }
    if(!all((from-to) %in% roll)) {
      print(paste("roll=",paste(roll)))
      print.board(before.board)
      print.board(after.board)
      print(from)
      print(to)
      print(paste("diff=",paste(from-to,collapse=",")))
      stop("should give roll values")
    }
  }
  desc=""
  for(i in 1:length(from)) desc=paste0(desc,"(",from[i],",",to[i],")")
  for(b in blots) desc=paste0(desc,"(",b,",",b.bar,")")
  return(desc)
}

# should produce a valid board configuration on each call
random.board=function(){
  board=rep(0,28)
  points=sample.int(26,15,replace=TRUE)
  for(p in points) board[p]=board[p]+1
  xboard=board
  xboard[c(25,26)]=1
  points=sample(which(xboard==0),15,replace=TRUE)
  for(p in points) board[p]=board[p]-1
  #print(sum(board))
  board
}

if(FALSE){
  # compute time to play N games
  start.time=Sys.time()
  for(i in 1:10000) play.game(verbose=FALSE)
  end.time=Sys.time()
  end.time-start.time
  
  verbose=TRUE
  history=play.game(verbose)
}

if(FALSE){
  roll.dice=roll.dice.cl()
  verbose=TRUE
  board=random.board()
  roll=roll.dice()
  #roll=c(1,1,1,1)
  moves=find.all.possible.moves(board,roll)
  duplicated(moves)
  print(length(moves))
  for(i in seq_along(moves)) {
    print(paste(paste0("[",i,"]"),paste(roll,collapse=","),describe.move(roll,board,moves[[i]])))
    print.board(board)
    print.board(moves[[i]])
    #print(describe.move(roll,board,moves[[i]]))
  }
  moves=unique(moves)
  print(length(moves))
  for(i in seq_along(moves)) {
    print(paste(paste0("[",i,"]"),paste(roll,collapse=","),describe.move(roll,board,moves[[i]])))
    print.board(board)
    print.board(moves[[i]])
    #print(describe.move(roll,board,moves[[i]]))
  }
}


if(FALSE){
  board=init.board()
  rolls=as.matrix(expand.grid(1:6,1:6))
  boards=list()
  for(i in 1:nrow(rolls)) {
    roll=c(rolls[i,1],rolls[i,2])
    if(roll[1]==roll[2]) roll=c(roll,roll)
    boards=c(boards,find.all.possible.moves(board,roll))
  }
  print(length(boards))
  boards=unique(boards)
  print(length(boards))
}

