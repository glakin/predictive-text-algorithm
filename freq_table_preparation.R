#### This script is used to prepare the freq_table.csv file that is used by the model


library(dplyr)
library(tidytext)
library(tidyr)
library(R.utils)
library(stringr)

# percentage of lines to read in
readpct <- 0.1

#texts
text_source = c("news","blogs","twitter")

# Build the dataframe
all_texts <- data.frame(matrix(ncol=3,nrow=3, 
                               dimnames=list(NULL, c("text", "source", "lines"))))
for (i in 1:length(text_source)) {
  directory <- paste0("data/en_US/en_US.",text_source[i],".txt")
  all_texts$lines[i] <- sapply(directory,countLines)        
  all_texts$text[i] <- paste(readLines(directory,
                                       round(readpct*all_texts$lines[i]), 
                                       encoding="UTF-8",
                                       skipNul = T),
                             collapse=" ")
  all_texts$source[i] <- text_source[i]
  
}

# Download list of bad words file if it doesn't exist
if(!file.exists('data/bad_words.txt')){
  url <- 'https://raw.githubusercontent.com/RobertJGabriel/Google-profanity-words/master/list.txt'
  download.file(url, 'data/bad_words.txt')
}

bad_words <- read.table('data/bad_words.txt')
#bad_words$V1 <- lapply(bad_words$V1, string_pad, ) 
#bad_words$V1 <- paste0()

all_texts <- all_texts %>%
  mutate( text = gsub(x=text, pattern = paste(bad_words$V1, collapse = ' | '), replacement = "")) %>%
  mutate( text = gsub(x=text, pattern = "[0-9]+", replacement = "")) %>%
  mutate( text = iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT"))

freq_table <- data.frame()

#Build a list of ngrams for n = 1:4
for (i in 1:4) {
  cols <- c("element", "frequency", "n")
  
  df1 <- all_texts %>% 
    unnest_tokens(element, text, token = "ngrams", n=i) %>%
    count(element, sort = TRUE)
  
  n <- rep(i, length(df1$element))
  
  df <- cbind(df1, n)
  colnames(df) <- cols
  
  freq_table <- rbind(freq_table, df)
  
}

freq_table$output <- unlist(lapply(freq_table$element, word, start=-1))
freq_table$input <- NA
freq_table[freq_table$n != 1,]$input <- unlist(lapply(freq_table[freq_table$n != 1,]$element, word, start=1, end=-2))

freq_table <- freq_table %>%
  relocate(input, .after=element) %>%
  relocate(output, .after=input) %>%
  subset(select = -(element))

# Drop all combinations that only occur once in the data set
# These represent a massive part of the table and we do not have high 
# confidence in them
freq_table <- freq_table[freq_table$frequency > 1,]

# Only include the top 10 1-grams
freq_table <- freq_table %>%
  filter(n > 1 | freq_table$output %in% head(freq_table[freq_table$n == 1,]$output,10))

write.csv(freq_table, file = "text_prediction_app/data/freq_table.csv", 
          row.names = FALSE)
