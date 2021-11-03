
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

all_texts <- all_texts %>%
  mutate( text = gsub(x=text, pattern = "[0-9]+", replacement = "")) %>%
  mutate( text = iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT"))

freq_table <- data.frame()

for (i in 1:5) {
  cols <- c("element", "frequency", "n")
  
  df1 <- all_texts %>% 
    unnest_tokens(element, text, token = "ngrams", n=i) %>%
    count(element, sort = TRUE)
  
  n <- rep(i, length(df1$element))
  
  df <- cbind(df1, n)
  colnames(df) <- cols
  
  freq_table <- rbind(freq_table, df)
  
}

freq_table$output <- lapply(freq_table$element, word, start=-1)
freq_table$input <- NA
freq_table[freq_table$n != 1,]$input <- lapply(freq_table[freq_table$n != 1,]$element, word, start=1, end=-2)

freq_table <- freq_table %>%
  relocate(input, .after=element) %>%
  relocate(output, .after=input)

#input_freq <- freq_table %>% 
#  group_by(input) %>% 
#  summarize(input_freq = sum(frequency))

#freq_table <- left_join(freq_table, input_freq, by=input, keep=FALSE)

ngram_model <- function(input_text, freq_table) {
  
  input_text <- gsub(x=input_text, pattern = "[0-9]+", replacement = "")
  input_text <- iconv(input_text, from = "UTF-8", to = "ASCII//TRANSLIT")
  
  len <- length(strsplit(input_text, " ")[[1]])
  
  max_n <- min(len + 1, max(freq_table$n))
  
  for (i in max_n:1) {
    input_trim <- word(input_text, start=-i+1, end=-1)
    
    df <- freq_table[freq_table$input==input_trim & freq_table$n==i,] %>%
      drop_na(element)
    
    if(length(df$element)>0) {
      df$probability <- df$frequency/sum(df$frequency)
      output_text <- sample(df$output, size=1, prob=df$probability )
      #output_text <- df[which.max(df$frequency),]$output
      #print(i)
      break
    } #else {
      # Laplace smoothing
      
   # }
  }
  
  return(output_text)
}


######## MARKOV CHAINS ###########

library(dplyr)
library(tidytext)
library(tidyr)
library(R.utils)
library(stringr)
library(markovchain)
library(R.utils)

readpct <- 0.1

twitter_path <- "data/en_US/en_US.twitter.txt"
blogs_path <- "data/en_US/en_US.blogs.txt"
news_path <- "data/en_us/en_US.news.txt"

twitter_lines <- countLines(twitter_path)
blogs_lines <- countLines(blogs_path)
news_lines <- countLines(news_path)

twitter <- readLines(twitter_path, round(readpct*twitter_lines))
blogs <- readLines(blogs_path, round(readpct*blogs_lines))
news <- readLines(news_path, round(readpct*news_lines))

text_df <- rbind(tibble(text = twitter),
                 tibble(text = blogs),
                 tibble(text = news)) %>%
  mutate(text = gsub(x=text, pattern = "[0-9]+", replacement = "")) %>%
  mutate(text = iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT"))

unigrams <- text_df %>%
  unnest_tokens(output = word, input = text)

unigram_markov <- markovchainFit(unigrams)
