
library(shiny)
library(bslib)

# Javascript code to run the prediction when the user presses space
js <- '
$(document).on("keyup", function(e) {
  if(e.keyCode == 32){
    Shiny.onInputChange("spacePressed", Math.random());
  }
});
'

# Javascript code to copy the new text into the input after a new word is chosen
js2 <- "
    Shiny.addCustomMessageHandler('input_text', function(value) {
    Shiny.setInputValue('input_text', value);
    });
  "

navbarPage("Shakespearean Text Prediction",
           
           theme = bs_theme(version = 4, bootswatch = "flatly"),         
           
           tabPanel("Text Prediction", 
                    
                    tags$script(js),
                    tags$script(js2),
                    
                    # Application title
                    titlePanel(h2("Shakespearean Text Prediction with Bard-ov Chains", align = "center")),
                    
                    #h3("Instructions", align = "center"),
                    tags$div("Start typing some text and recommendations will populate below. 
             Click on a suggestion to add it to the text.",
                             align = "center"),
                    
                    fluidRow(
                        column(6, offset = 3, align = "center",
                               #textInput("input_text", NULL, placeholder = "Start typing!",
                               uiOutput('textInput'),
                               fluidRow(
                                   column(4,
                                          uiOutput( 'predictionButton1' )),
                                   column(4,
                                          uiOutput( 'predictionButton2' )),
                                   column(4,
                                          uiOutput( 'predictionButton3' ))
                               )
                        )
                    ),
           ),
           tabPanel("About",
                    
                    img(src="shakespeare.jpg", align = "center")
                    
           )
)
