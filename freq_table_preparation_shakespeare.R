library(dplyr)
library(stringr)

if(!file.exists("data/shakespeare.txt")){
  download.file("https://ocw.mit.edu/ans7870/6/6.006/s08/lecturenotes/files/t8.shakespeare.txt", "data/shakespeare.txt")
}

all_text <- readLines("data/shakespeare.txt")

# Trim the preamble
all_text <- all_text[247:length(all_text)-19]

all_text <- tolower(all_text)

all_text_df <- data.frame(text = all_text)

all_text_df <- all_text_df %>%
  mutate( text = gsub(x=text, pattern = "[0-9]+", replacement = "")) %>%
  mutate( text = gsub(x=text, pattern = "William Shakespeare", replacement = "")) %>%
  mutate( text = iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT"))


freq_table <- data.frame()

#Build a list of ngrams for n = 1:4
for (i in 1:5) {
  cols <- c("element", "frequency", "n")
  
  df1 <- all_text_df %>% 
    unnest_tokens(element, text, token = "ngrams", n=i) %>%
    count(element, sort = TRUE)
  
  n <- rep(i, length(df1$element))
  
  df <- cbind(df1, n)
  colnames(df) <- cols
  
  freq_table <- rbind(freq_table, df)
  
}


# Drop all combinations that only occur once in the data set
# These represent a massive part of the table and we do not have high 
# confidence in them
freq_table <- freq_table[freq_table$frequency > 1,] %>%
  drop_na()

freq_table$output <- unlist(lapply(freq_table$element, word, start=-1))
freq_table$input <- NA
freq_table[freq_table$n != 1,]$input <- unlist(lapply(freq_table[freq_table$n != 1,]$element, word, start=1, end=-2))

freq_table <- freq_table %>%
  relocate(input, .after=element) %>%
  relocate(output, .after=input) %>%
  subset(select = -(element))

# Only include the top 10 1-grams
freq_table <- freq_table %>%
  filter(n > 1 | freq_table$output %in% head(freq_table[freq_table$n == 1,]$output,100))

write.csv(freq_table, file = "shakespeare_text_prediction_app/data/freq_table_shakespeare.csv", 
          row.names = FALSE)
