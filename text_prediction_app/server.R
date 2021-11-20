#### TEXT PREDITICTION APP ####
#### Jerry Lakin - Nov 2021 ####

library(shiny)

shinyServer(function(input, output) {

    freq_table <- read.csv('data/freq_table.csv')
    
    ngram_model <- function(input_text, freq_table) {
        
        input_text <- gsub(x=input_text, pattern = "[0-9]+", replacement = "")
        input_text <- iconv(input_text, from = "UTF-8", to = "ASCII//TRANSLIT")
        
        len <- length(strsplit(input_text, " ")[[1]])
        
        max_n <- min(len + 1, max(freq_table$n))
        
        for (i in max_n:1) {
            input_trim <- word(input_text, start=-i+1, end=-1)
            
            df <- freq_table[freq_table$input==input_trim & freq_table$n==i,] %>%
                drop_na(output)
            
            if(length(df$input)>0) {
                df$probability <- df$frequency/sum(df$frequency)
                output_text <- sample(df$output, size = 1, prob = df$probability )
                #output_text <- df[which.max(df$frequency),]$output
                #print(i)
                break
            } else {
                df0 <- freq_table[freq_table$n == 1 ,]
                output_text <- sample(df0$output, size = 1, prob = df0$probability)
                
            }
        }
        
        return(output_text)
    }
    
    observeEvent(input$input_text, ignoreInit = TRUE, {
        
        prediction <- ngram_model(input$input_text, freq_table)
        
        output$textPrediction <- renderText({prediction}) 
    })
    
    

})
