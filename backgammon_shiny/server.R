#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("backgammon_board.R")
source("board.R")


board <- init.black() + init.white()
print("initial board")
str(board)

playerB <- TRUE 
move <- TRUE

shinyServer(function(input, output) {

    # xpoints = c(3,47) by 4, ylim = c(7,5)&c(3,1) by 0.5
    # df_board created in board.R 

    #np = nearPoints(df_board, input$plot1_click, xvar = "x", yvar = "y")
    #errorCode <- reactiveValues(data = NULL)
    observeEvent(input$newGame, {
       # board <<- init.black() + init.white()
       # playerB <<- TRUE 
       # move <<- TRUE
    #errorCode$data <- TRUE
    })
    
     selected <- reactive({
         # add clicked
         selected_points <<-  nearPoints(df_board, input$plot1_click, xvar = "x", yvar = "y")

        #print("selected points: ")
       # print(selected_points)
        
         return(selected_points)
     })

    output$click_info <- renderPrint({ turn(selected()) } )
    output$plot1 <- renderPlot({
        board_plot(selected())
    })

})
