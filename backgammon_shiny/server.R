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
error <<- F
playerB <<- TRUE 
move <<- TRUE
value <<- -200
valueTurn <<- -200
valueAIturn <<- -200
valueRturn <<- -200
valueAIB <<- -200
inactive <<- FALSE
pick <<- 0
roll <<- roll.dice()
roll.track <<- roll
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
        inactive <<- TRUE
        value <<- input$newGame
        roll <<- roll.dice()
        roll.track <<- roll
        }
        
    })
    
    # Next player's turn
    observeEvent(input$turn, {
        if (input$turn != valueTurn){
            
            # TO DO: HIGHLIGHT POSSIBLE MOVES
            # TO DO: CHECK THAT MOVES ARE LEGAL
            inactive <<- TRUE
            playerB <<- !playerB
            move <<- TRUE
            roll <<- roll.dice()
            roll.track <<- roll
            valueTurn <<- input$turn
        }
        if(game.over(board) || game.over(flip.board(board))){
            showModal(modalDialog(
                title = "Important message",
                "Game over!",easyClose = TRUE
            ))
        }

    })
    
    # AI's turn
    observeEvent(input$AIturn, {
        if (input$AIturn != valueAIturn){
            playerB <<- !playerB
            move <<- TRUE
            roll <<- roll.dice()
            valueAIturn <<- input$AIturn
            inactive <<- TRUE
            roll.track <<- 0
            # PLACE HOLDER
                if(possible()){
                print("MOVES POSSIBLE")
                print(ai_move(board, roll, playerB))
                board <<- ai_move(board, roll, playerB)
                    if(game.over(board) || game.over(flip.board(board))){
                        showModal(modalDialog(
                            title = "Important message",
                            "Game over!",easyClose = TRUE
                        ))
                    }
                }
                else{
                    print("NO POSSIBLE MOVES")
                    showModal(modalDialog(
                        title = "Important message",
                        "No possible moves!",easyClose = TRUE
                    ))
                }
        }
        
    })
    
    # AI B's turn
    observeEvent(input$AIBturn, {
        if (input$AIBturn != valueAIB){
            playerB <<- !playerB
            move <<- TRUE
            roll <<- roll.dice()
            valueAIB <<- input$AIBturn
            inactive <<- TRUE
            roll.track <<- 0
            # PLACE HOLDER
            if(possible()){
                print("MOVES POSSIBLE")
                print(aiB_move(board, roll, playerB))
                board <<- aiB_move(board, roll, playerB)
                if(game.over(board) || game.over(flip.board(board))){
                    showModal(modalDialog(
                        title = "Important message",
                        "Game over!",easyClose = TRUE
                    ))
                }
            }
            else{
                print("NO POSSIBLE MOVES")
                showModal(modalDialog(
                    title = "Important message",
                    "No possible moves!",easyClose = TRUE
                ))
            }
        }
        
    })
    
    # Random turn
    observeEvent(input$Rturn, {
        if (input$Rturn != valueRturn  && possible()){
            playerB <<- !playerB
            move <<- TRUE
            roll <<- roll.dice()
            valueRturn <<- input$Rturn
            inactive <<- TRUE
            roll.track <<- 0
            if(possible()){
            board <<- random_turn()
                if(game.over(board) || game.over(flip.board(board))){
                    showModal(modalDialog(
                        title = "Important message",
                        "Game over!",easyClose = TRUE
                    ))
                }
            }
            else{
                print("NO POSSIBLE MOVES")
                showModal(modalDialog(
                    title = "Important message",
                    "No possible moves!",easyClose = TRUE
                ))
            }
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
        print("board sum:")
        print(sum(board))
        board_plot(selected())
        inactive <<- FALSE
        
    })
    

    
    output$click_info <- renderPrint({ turn(selected()) })
    

})
