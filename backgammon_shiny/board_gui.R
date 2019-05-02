tt <- tktoplevel()
bb<-1
img <-tkrplot(tt, function() plot(1:20,(1:20)^bb))
f<-function(...) {
  b <- as.numeric(tclvalue("bb"))
  if (b != bb) {
    bb <<- b
    tkrreplot(img)
  }
}
s <- tkscale(tt, command=f, from=0.05, to=2.00, variable="bb",
             showvalue=FALSE, resolution=0.05, orient="horiz")
tkpack(img,s)



#--------------------------------------------------------
tt <- tktoplevel()
tkwm.title(tt, "Sudoku")
img <- tkrplot(tt, replot, hscale = hscale, vscale = vscale)
txt <- tktext(tt, bg = "white", font = "courier")
scr <- tkscrollbar(tt, repeatinterval = 5, command = function(...) tkyview(txt, 
                                                                           ...))
tkconfigure(txt, yscrollcommand = function(...) tkset(scr, 
                                                      ...))
tkpack(img, side = "top")
tkpack(txt, side = "left", fill = "both", expand = TRUE)
tkpack(scr, side = "right", fill = "y")
iw <- as.numeric(tcl("image", "width", tkcget(img, "-image")))
ih <- as.numeric(tcl("image", "height", tkcget(img, "-image")))

#--------------------------------------------------------
# April 17, 2019
# Fiona paine
# ** code inspired by sudoku R package
# Board is 28 long with the last 4 off the board. 
# indexes 1 through 14 starting in 
# bottom right corner (white home)
# positive numbers are white
# negative numbers are black
# each index is a location that denotes
# the numer of pieces at location
# 25=white bar, 26=white off, 27=black off, 28=black bar
require(tkrplot)

init.white()
init.black()

board = init.black() + init.white()
## End(Not run)

board_static <- function(board){
tt <- tktoplevel()
tkwm.title(tt, "AI Backgammon")
f <- function() {
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
  print(paste("bottom ",loc))
  for (j in 1:num){
  points(x = loc, y = .5+(j*.5), pch = 19, cex = 2, col =colP)
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
    print(paste("top",loc))
    for (j in 1:num){
      points(x = loc, y = 7.5-j*.5, pch = 19, cex = 2, col = colP)
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
img <- tkrplot(tt, f())
tkpack(img)
}

board_gui <- function(board){
  tt <- tktoplevel()
  tkwm.title(tt, "AI Backgammon")
  f <- function() {
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
        print(paste("bottom ",loc))
        for (j in 1:num){
          points(x = loc, y = .5+(j*.5), pch = 19, cex = 2, col =colP)
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
        print(paste("top",loc))
        for (j in 1:num){
          points(x = loc, y = 7.5-j*.5, pch = 19, cex = 2, col = colP)
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
  img <- tkrplot(tt, f())

txt <- tktext(tt, bg = "white", font = "courier")
scr <- tkscrollbar(tt, repeatinterval = 5, command = function(...) tkyview(txt, 
                                                                           ...))
tkconfigure(txt, yscrollcommand = function(...) tkset(scr, 
                                                      ...))
tkpack(img, side = "top")
tkpack(txt, side = "left", fill = "both", expand = TRUE)
tkpack(scr, side = "right", fill = "y")

# Displays Text
type <- function(s) {
  tkinsert(txt, "end", s)
  tksee(txt, "end")
}
type("Welcome! Please make a move by 1) clicking the piece you want to move.")
type( "2) Click the new piece location ")


mouseclick <- function(buttons, x, y){
  c(x, y)
  return()
}
# cc <- function(x, y) {
#   
#     x <- (as.double(x) - 1)/iw
#     y <- 1 - (as.double(y) - 1)/ih
#   
#   px <- (x - cplt[1])/(cplt[2] - cplt[1])
#   py <- (y - cplt[3])/(cplt[4] - cplt[3])
#   ux <- px * (cusr[2] - cusr[1]) + cusr[1]
#   uy <- py * (cusr[4] - cusr[3]) + cusr[3]
#   c(10 - round(uy), round(ux))
# }
getGraphicsEvent(prompt = "Waiting for move.", onMouseDown = mouseclick)
#eventEnv <- getGraphicsEventEnv()
tkwait.window(tt)
}


board_static(board)

