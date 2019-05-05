#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# https://shiny.rstudio.com/gallery/plot-interaction-selecting-points.html

library(shiny)

# Define UI for application
shinyUI(fluidPage(

    # Application title
    titlePanel("Backgammon"),
    fluidRow(
        column(width = 8,
               h4("Game instructions"),
               p("1. After selecting a button you must click inside the board to complete the action. (This includes after dismissing a popup message)"),
               p("2. After clicking Random turn/AI turn you must click == Next Player's Turn == for your turn."),
               p("3. White moves counterclockwise and Black moves clockwise.")
               )
    ),
 #   br(),

    fluidRow(
        column(width = 4,
               plotOutput("plot1", height = 600,width = 600,
                          # Equivalent to: click = clickOpts(id = "plot_click")
                          click = "plot1_click"
               )
        )
    )
    ,

 actionButton('newGame','New Game'),
 actionButton('turn','Next player\'s turn'),
 actionButton('Rturn','Random turn'),
 actionButton('AIturn','AI\'s turn'),
 actionButton('AIBturn','AI B\'s turn'),

    fluidRow( br(),
        column(width = 8,
               verbatimTextOutput("click_info")
        ),
        br()
        
    )
 #,fluidRow(p("Created by: Fiona Paine  ", align = "center"))
    

)
)