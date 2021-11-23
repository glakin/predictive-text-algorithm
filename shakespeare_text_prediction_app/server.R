#### TEXT PREDITICTION APP ####
#### Jerry Lakin - Nov 2021 ####

library(shiny)
library(stringr)
library(dplyr)
library(tidyr)

shinyServer(function(input, output, session) {
    
    # Read the freq table which will be used in the ngram model
    freq_table <- read.csv("data/freq_table_shakespeare.csv")
    
    predictions <- reactiveValues()
    
    # Initialize
    output$textInput <- renderUI({
        textInput("input_text", NULL, placeholder = "Start typing!", width = "100%")
        # Experimenting with using a text area instead of a textInput object
        #tags$textarea(id = "input_text", rows=3, "Start typing", width = "100%")
    })
    
    # Function to predict a word based on inputs
    # input_text = string to predict the next word of
    # freq_table = table of n-gram frequencies prepared in freq_table_preparation.R
    # exclude = words to exlude from potential outputs
    ngram_model <- function(input_text, freq_table, exclude) {
        
        input_text <- gsub(x=input_text, pattern = "[0-9]+", replacement = "")
        input_text <- iconv(input_text, from = "UTF-8", to = "ASCII//TRANSLIT")
        input_text <- trimws(input_text, which = "both")
        
        len <- length(strsplit(input_text, " ")[[1]])
        
        # Minimum length of 1 - if function is evaluated for an empty string
        # it will return a 1-gram
        if(len == 0){
            len = 1
        }
        
        max_n <- min(len + 1, max(freq_table$n))
        
        output_text <- NA
        
        for(i in max_n:2) {
            
            # Find the final words that will be used to search for corresponding
            # ngrams. Only include the final x words where x = n - 1.
            input_trim <- word(input_text, start=-i+1, end=-1)
            
            # Find ngrams corresponding to final words
            df <- freq_table[freq_table$input==input_trim & freq_table$n==i,] %>%
                drop_na(output) %>%
                filter(!(output %in% exclude))
            
            # If matching rows are found sample an output word and break the loop
            if(length(df$input)>0) {
                df$probability <- df$frequency/sum(df$frequency)
                output_text <- sample(df$output, size = 1, prob = df$probability)
                break
            } 
        }
        
        # If no words are found from the ngram loop, sample from the top most
        # common words in the set
        if(is.na(output_text)) {
            df0 <- freq_table[freq_table$n == 1 ,]
            output_text <- sample(df0$output, size = 1, prob = df0$probability)
        }
        
        return(output_text)
    }
    
    # Generate predictions when the space is pressed
    observeEvent(input$spacePressed, ignoreInit = TRUE, {
        
        exclude <- c()
        
        # Predict a word, add it to exclusions and predict again up to 3 words
        predictions$pred1 <- ngram_model(input$input_text, freq_table, exclude)
        exclude <- append(exclude, predictions$pred1)
        predictions$pred2 <- ngram_model(input$input_text, freq_table, exclude)
        exclude <- append(exclude, predictions$pred2)
        predictions$pred3 <- ngram_model(input$input_text, freq_table, exclude)
        
        #Update the 3 buttons containing recommended words
        output$predictionButton1 <- renderUI({
            customLabel <- predictions$pred1
            actionButton("clickPredict1", label = customLabel, width = "100%")
        })
        
        output$predictionButton2 <- renderUI({
            customLabel <- predictions$pred2
            actionButton("clickPredict2", label = customLabel, width = "100%")
        })
        
        output$predictionButton3 <- renderUI({
            customLabel <- predictions$pred3
            actionButton("clickPredict3", label = customLabel, width = "100%")
        })
        
    })
    
    # Update the text and generate new predictions if button 1 is pressed
    observeEvent(input$clickPredict1, ignoreInit = TRUE, {
        
        exclude <- c()
        
        # Append the selected word to the input text
        oldText <- trimws(input$input_text, which = "both")
        newText <- paste(oldText, predictions$pred1)
        
        # Update the words in the input text box
        output$textInput <- renderUI({
            textInput("input_text", NULL, value = newText, width = "100%")
        })
        
        #Predict a word, add it to exclusions and predict again up to 3 words
        predictions$pred1 <- ngram_model(newText, freq_table, exclude)
        exclude <- append(exclude, predictions$pred1)
        predictions$pred2 <- ngram_model(newText, freq_table, exclude)
        exclude <- append(exclude, predictions$pred2)
        predictions$pred3 <- ngram_model(newText, freq_table, exclude)
        
        #Update the 3 buttons containing recommended words
        output$predictionButton1 <- renderUI({
            customLabel <- predictions$pred1
            actionButton("clickPredict1", label = customLabel, width = "100%")
        })
        
        output$predictionButton2 <- renderUI({
            customLabel <- predictions$pred2
            actionButton("clickPredict2", label = customLabel, width = "100%")
        })
        
        output$predictionButton3 <- renderUI({
            customLabel <- predictions$pred3
            actionButton("clickPredict3", label = customLabel, width = "100%")
        })
    })
    
    # Update the text and generate new predictions if button 2 is pressed
    observeEvent(input$clickPredict2, ignoreInit = TRUE, {
        
        exclude <- c()
        
        # Append the selected word to the input text
        oldText <- trimws(input$input_text, which = "both")
        newText <- paste(oldText, predictions$pred2)
        
        # Update the words in the input text box
        output$textInput <- renderUI({
            textInput("input_text", NULL, value = newText, width = "100%")
        })
        
        #Predict a word, add it to exclusions and predict again up to 3 words
        predictions$pred1 <- ngram_model(newText, freq_table, exclude)
        exclude <- append(exclude, predictions$pred1)
        predictions$pred2 <- ngram_model(newText, freq_table, exclude)
        exclude <- append(exclude, predictions$pred2)
        predictions$pred3 <- ngram_model(newText, freq_table, exclude)
        
        #Update the 3 buttons containing recommended words
        output$predictionButton1 <- renderUI({
            customLabel <- predictions$pred1
            actionButton("clickPredict1", label = customLabel, width = "100%")
        })
        output$predictionButton2 <- renderUI({
            customLabel <- predictions$pred2
            actionButton("clickPredict2", label = customLabel, width = "100%")
        })
        
        output$predictionButton3 <- renderUI({
            customLabel <- predictions$pred3
            actionButton("clickPredict3", label = customLabel, width = "100%")
        })
    })
    
    # Update the text and generate new predictions if button 1 is pressed
    observeEvent(input$clickPredict3, ignoreInit = TRUE, {
        
        exclude <- c()
        
        # Append the selected word to the input text
        oldText <- trimws(input$input_text, which = "both")
        newText <- paste(oldText, predictions$pred3)
        
        # Update the words in the input text box
        output$textInput <- renderUI({
            textInput("input_text", NULL, value = newText, width = "100%")
        })
        
        #Predict a word, add it to exclusions and predict again up to 3 words
        predictions$pred1 <- ngram_model(newText, freq_table, exclude)
        exclude <- append(exclude, predictions$pred1)
        predictions$pred2 <- ngram_model(newText, freq_table, exclude)
        exclude <- append(exclude, predictions$pred2)
        predictions$pred3 <- ngram_model(newText, freq_table, exclude)
        
        #Update the 3 buttons containing recommended words
        output$predictionButton1 <- renderUI({
            customLabel <- predictions$pred1
            actionButton("clickPredict1", label = customLabel, width = "100%")
        })
        
        output$predictionButton2 <- renderUI({
            customLabel <- predictions$pred2
            actionButton("clickPredict2", label = customLabel, width = "100%")
        })
        
        output$predictionButton3 <- renderUI({
            customLabel <- predictions$pred3
            actionButton("clickPredict3", label = customLabel, width = "100%")
        })
    })
    
    
    
    
    
    
})
