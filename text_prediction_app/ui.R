
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

navbarPage("Data Science Capstone",
  
           theme = bs_theme(version = 4, bootswatch = "flatly"),         
  
  tabPanel("Text Prediction", 
    
    tags$script(js),
    tags$script(js2),
    
    # Application title
    titlePanel(h1("Markov Chain N-Gram Text Prediction Algorithm", align = "center")),

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
  
    tags$h3("About the Application"),
    tags$div("This application was created by Jerry Lakin in November 2021 for the 
             capstone project of the ",
             tags$a(href="https://www.coursera.org/specializations/jhu-data-science",
                    "Johns Hopkins Data Science Specialization on Coursera."),
             "The data was provided by SwiftKey.",
             br(), br(),
             "All of the code is available in the",
             tags$a(href="https://github.com/glakin/predictive-text-algorithm",
                    "Github repo."),
             br(), br(),
             "An analysis of the data is available on",
             tags$a(href="https://rpubs.com/glakin/827608", "RPubs.")
            ),
    br(),
    tags$h3("How it Works"),
    tags$div("The application makes text predictions using an n-gram model.",
             br(), br(),
             "The data used to build the model was provided by SwiftKey and
             contained roughly 4,000,000 lines of English text from news
             articles, blogs and Twitter. After formatting and removing all
             profanity, a sample of 10% of these lines was
             used to construct a corpus. The corpus was then tokenized into
             n-grams with n = 1...4 and the frequency of n-grams were calculated.",
             br(), br(),
             "The model accepts input text, isolates the final three words
             in the input, and searches for 4-gram matches that begin with those
             words. If it fails to find a match, it scans for 3-gram matches
             and so on. When the model finds at least one possible n-gram
             corresponding to the last words of the input text, it suggests
             a word using the relative frequency of the n-gram as a probability
             so that more common n-grams are utilized more frequently.",
             br(), br(),
             "In order to manage the size of the dataset and reduce noise, n-grams
             with only a single occurence were discarded from the results. 
             Additionally, only the top 10 1-grams were retained. This is because
             the 1-grams are only used when the model cannot find any n-grams
             corresponding to the input text. In these situations it makes
             the most sense to recommend a very common word."
             )
    
                    
  )
)
