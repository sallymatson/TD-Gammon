
df_board.x = NULL
df_board.y = NULL
x.points = seq(3,47, by=4)
for (i in x.points){
  df_board.x = c(df_board.x, rep(i, times = 10))
  df_board.y = c(df_board.y, seq(5,7,by=.5), seq(1,3,by=.5))
}
df_board = data.frame(cbind(df_board.x,df_board.y))
colnames(df_board)<- c("x", "y")

require(plyr)
# initialize board global variable



turn <- function(selected_points) {
  x.move = selected_points$x
  y.move = selected_points$y
  valid = !empty(selected_points)
  

    
  if (playerB && move){
    # tracking whose turn
    
    return("Player Black should select piece to move")
  }
  else if (!playerB && move) {
    # tracking whose turn
    
    return("Player White should select piece to move")
  }
  
  else if (playerB && !move) {
    if (valid){
    playerB <<- !playerB
    }
    return("Player Black should select where to place the piece")
  }
  
  else if (!playerB && !move) {
    if (valid){
    playerB <<- !playerB
    }
    return("Player White should select where to place the piece")
  }
  
    }



board_update <- function(selected_points) {
  
  x.move = selected_points$x
  y.move = selected_points$y
  
  # TO DO check choice is valid
  #valid = check_choice(selected_points)
  valid = !empty(selected_points)
  if(valid){
    
    # xpoints = c(3,47) by 4, ylim = c(7,5)&c(3,1) by 0.5
    # top of board::: loc = (i-13)*4 + 3 [solve for i]
    
    if (move){  # we are adding to selected location
      move <<- !move
    if(y.move >=5){
      index = 0.25*(x.move - 3) + 13
      if (board[index] > 0){
        board[index] = board[index] + 1
      }
      else if(board[index] < 0){
        board[index] = board[index] - 1
      }
      else {
        if (playerB){
          board[index] = board[index] - 1
        }
        else{
          board[index] = board[index] + 1
        }
      }
      
      print(paste("TOP: ", index))
    }
    
    # bottom of board::: loc = 50 - ((i-1)*4 + 3)  [solve for i]
    else {
      index = 1 - (x.move - 47)*0.25
      if (board[index] > 0){
        board[index] = board[index] + 1
      }
      else if(board[index] < 0){
        board[index] = board[index] - 1
      }
      else {
        if (playerB){
          board[index] = board[index] - 1
        }
        else{
          board[index] = board[index] + 1
        }
        print(paste("BOTTOM: ", index))
    }
      
    print("New Board ")
    print(board)
    return(board)
    
    }
    }
    
    else{
      move <<- !move
      if(y.move >=5){
        index = 0.25*(x.move - 3) + 13
        if (board[index] > 0){
          board[index] = board[index] - 1
        }
        else if(board[index] < 0){
          board[index] = board[index] + 1
        }
        else {
          if (playerB){
            board[index] = board[index] + 1
          }
          else{
            board[index] = board[index] - 1
          }
        }
        
        print(paste("TOP: ", index))
      }
      
      # bottom of board::: loc = 50 - ((i-1)*4 + 3)  [solve for i]
      else {
        index = 1 - (x.move - 47)*0.25
        if (board[index] > 0){
          board[index] = board[index] + 1
        }
        else if(board[index] < 0){
          board[index] = board[index] - 1
        }
        
        else {
          if (playerB){
            board[index] = board[index] - 1
          }
          else{
            board[index] = board[index] + 1
          }
        }
        print(paste("BOTTOM: ", index))
      }
      
      print("New Board ")
      print(board)
      return(board)
      
    }
      
  }
  
  else {
    return(board)
  }
}

 board_plot <- function(selected) {
   
    board <<- board_update(selected)
   

   
    yB = c(1,2,3,2,1 )
    yT = c(7,6,5,6,7 )
    plot(x = c(1,2,3,4,5) , y = yB, type = "l", col = c("red"), 
         xlim = c(1,50), ylim = c(1,7), lwd = 4, tck=0,
         ylab = "", xlab = "")
    
    #Placing pieces on the board
    
    for (i in 1:12){
      if (board[i] != 0){
        if (board[i] > 0){
          colP = "lightgrey"
        }
        if (board[i] < 0){
          colP = "black"
        }
        num = abs(board[i])
        loc = 50 - ((i-1)*4 + 3)
        #print(paste("bottom ",loc))
        for (j in 1:num){
          points(x = loc, y = .5+(j*.5), pch = 19, cex = 3, col =colP)
        }
      }
    }
    
    for (i in 13:24){
      if (board[i] != 0){
        
        if (board[i] > 0){
          colP = "lightgrey"
        }
        if (board[i] < 0){
          colP = "black"
        }
        num = abs(board[i])
        loc = (i-13)*4 + 3
        #print(paste("top",loc))
        for (j in 1:num){
          points(x = loc, y = 7.5-j*.5, pch = 19, cex = 3, col = colP)
        }
      }
    }
    
    
    lines(x = 5:9, y = yB, col = "blue", lwd = 4)
    lines(x = 9:13, y = yB, col = "red", lwd = 4)
    lines(x = 13:17, y = yB, col = "blue", lwd = 4)
    lines(x = 17:21, y = yB, col = "red", lwd = 4)
    lines(x = 21:25, y = yB, col = "blue", lwd = 4)
    
    lines(x = 1:5, y = yT, col = "blue", lwd = 4)
    lines(x = 5:9, y = yT, col = "red", lwd = 4)
    lines(x = 9:13, y = yT, col = "blue", lwd = 4)
    lines(x = 13:17, y = yT, col = "red", lwd = 4)
    lines(x = 17:21, y = yT, col = "blue", lwd = 4)
    lines(x = 21:25, y = yT, col = "red", lwd = 4)
    # Horizontal Line -- half way
    lines(x = rep(25, times = 9), y = 0:8, col = "black", lwd = 4)
    
    lines(x = (1:5)+24, y = yB, col = "red", lwd = 4)
    lines(x = (5:9)+24, y = yB, col = "blue", lwd = 4)
    lines(x = (9:13)+24, y = yB, col = "red", lwd = 4)
    lines(x = (13:17)+24, y = yB, col = "blue", lwd = 4)
    lines(x = (17:21)+24, y = yB, col = "red", lwd = 4)
    lines(x = (21:25)+24, y = yB, col = "blue", lwd = 4)
    
    lines(x = (1:5)+24, y = yT, col = "blue", lwd = 4)
    lines(x = (5:9)+24, y = yT, col = "red", lwd = 4)
    lines(x = (9:13)+24, y = yT, col = "blue", lwd = 4)
    lines(x = (13:17)+24, y = yT, col = "red", lwd = 4)
    lines(x = (17:21)+24, y = yT, col = "blue", lwd = 4)
    lines(x = (21:25)+24, y = yT, col = "red", lwd = 4)
    
    
 }
 