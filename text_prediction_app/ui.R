
library(shiny)
library(bslib)


shinyUI(fluidPage(
    
    theme = bs_theme(version = 4, bootswatch = "lumen"),
    
    # Application title
    titlePanel(h1("Text Prediction App", align = "center")),

    br(),
    
    fluidRow(
        column(10, offset = 1,
               textInput("input_text", NULL)),
        ),
    
    br(),
    
    h3(textOutput("textPrediction"), align = "center")
        
    
    
    
))
