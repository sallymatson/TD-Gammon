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

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Backgammon"),
 #   br(),
    actionButton('newGame','New Game'),
    fluidRow(
        column(width = 4,
               plotOutput("plot1", height = 600,width = 600,
                          # Equivalent to: click = clickOpts(id = "plot_click")
                          click = "plot1_click"
               )
        )
    )
    ,
    fluidRow(
        column(width = 8,
               h4("Game instructions"),
               verbatimTextOutput("click_info")
        )
    )
    
))
