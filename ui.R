#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Natural language prediction of the next word"),
  h5("The goal of this project is to predict the next possible word after you input several words"),
  h5("Please input some words, and set the maximum possible output number"),
  h5("The output will be presented as a sorted table, with mostly likely words on the top"),
  h5("A word cloud picture will be generated based on the probability"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("outputnum","Number of possible results:",min = 1,max = 20,value = 3),
       textInput("text", "Enter the words that you want to run the prediction", value = "i hope you have a great")
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       h4(textOutput("mostlikelyword")),
       h4(textOutput("othertitle")),
       h4(textOutput("otherword")),
       dataTableOutput("table"),
       plotOutput("wordcloudplot")
    )
  )
))
