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
source("ai_agent.R")


board <<-init.black() + init.white()
print("initial board")
str(board)

playerB <<- TRUE 
move <<- TRUE
value <<- -200
valueTurn <<- -200
valueAIturn <<- -200
valueRturn <<- -200
roll <<- roll.dice()
shinyServer(function(input, output) {

    # xpoints = c(3,47) by 4, ylim = c(7,5)&c(3,1) by 0.5
    # df_board created in board.R 

    #np = nearPoints(df_board, input$plot1_click, xvar = "x", yvar = "y")
    #errorCode <- reactiveValues(data = NULL)
    observeEvent(input$newGame, {
        if (input$newGame != value){
        board <<- init.black() + init.white()
        playerB <<- TRUE 
        move <<- TRUE
        value <<- input$newGame
        }
        
    })
    
    # Next player's turn
    observeEvent(input$turn, {
        if (input$turn != valueTurn){
            playerB <<- !playerB
            move <<- TRUE
            roll <<- roll.dice()
            valueTurn <<- input$turn
        }
    })
    
    # AI's turn
    observeEvent(input$AIturn, {
        if (input$AIturn != valueAIturn){
            playerB <<- !playerB
            move <<- TRUE
            roll <<- roll.dice()
            valueAIturn <<- input$AIturn
            # PLACE HOLDER
            board <<- init.black() + init.white()
        }
    })
    
    # Random turn
    observeEvent(input$Rturn, {
        if (input$Rturn != valueRturn){
            playerB <<- !playerB
            move <<- TRUE
            roll <<- roll.dice()
            valueRturn <<- input$Rturn
            board <<- random_turn()
        }
    })
    
     selected <- reactive({
         # add clicked
         selected_points <<-  nearPoints(df_board, input$plot1_click, xvar = "x", 
                                         yvar = "y", threshold = 8)

        #print("selected points: ")
       # print(selected_points)
        
         return(selected_points)
     })

    
    output$plot1 <- renderPlot({
        board_plot(selected())
    })
    output$click_info <- renderPrint({ turn(selected()) })

})
