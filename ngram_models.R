
library(utils)
library(dplyr)
library(tidytext)
library(tidyr)
library(R.utils)

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
                                       encoding="UTF-8"),collapse=" ")
  all_texts$source[i] <- source_list[i]

}

all_texts <- all_texts %>%
  mutate( text = gsub(x=text, pattern = "[0-9]+", replacement = "")) %>%
  mutate( text = iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT"))

freq_table <- data.frame()

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