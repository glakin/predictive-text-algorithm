

twitter <- readLines("data/en_US/en_US.twitter.txt")
blogs <- readLines("data/en_US/en_US.blogs.txt")
news <- readLines("data/en_us/en_US.news.txt")

#max1 = 0
#max2 = 0
#max3 = 0

#n1 = 0
#n2 = 0
#n3 = 0

#for (i in 1:length(twitter)) {
#  if (nchar(twitter[i]) > max1) {
#    max1 = nchar(twitter[i])
#    n1 = i
#  }
#}

#for (i in 1:length(blogs)) {
#  if (nchar(blogs[i]) > max2) {
#   max2 = nchar(blogs[i])
#    n2 = i
#  }
#}

#for (i in 1:length(news)) {
#  if (nchar(news[i]) > max3) {
#    max3 = nchar(news[i])
#    n3 = i
#  }
#}

#love <- length(grep("love", twitter))
#hate <- length(grep("hate", twitter))

##twitter[grep("biostats", twitter)]

#grep("A computer once beat me at chess, but it was no match for me at kickboxing", twitter)

#library(tm)


twitter_sample <- twitter[1:20000]
blogs_sample <- blogs[1:20000]
news_sample <- news[1:20000]

#twitter_corpus <- VCorpus(VectorSource(twitter_sample))
#twitter_corpus <- tm_map(twitter_corpus, content_transformer(tolower))
#twitter_corpus <- tm_map(twitter_corpus, removeWords, stopwords("english"))

#twitter_dtm <- DocumentTermMatrix(twitter_corpus)
#Find top words appearing
#twitter_top_terms <- findFreqTerms(twitter_dtm, 2000)

#inspect(twitter_dtm)
#findAssocs(twitter_dtm, "love", 0.8)

library(tidytext)
library(dplyr)
library(Rcpp)
library(stopwords)
library(stringr)

#custom_stopwords = (tibble(word = c("iâ", "itâ", "ã", "â", "å")))

#Twitter data
twitter_df <- tibble(Text = twitter_sample) %>%
  mutate( Text = gsub(x=Text, pattern = "[0-9]+", replacement = "")) %>%
  mutate( Text = iconv(Text, from = "UTF-8", to = "ASCII//TRANSLIT"))

twitter_wordcounts <- twitter_df %>%
  unnest_tokens(output = word, input = Text )%>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

head(twitter_wordcounts, n=20)


#Blog data
blog_df <- tibble(Text = blogs )%>%
  mutate( Text = gsub(x=Text, pattern = "[0-9]+", replacement = "")) %>%
  mutate( Text = iconv(Text, from = "UTF-8", to = "ASCII//TRANSLIT"))

blog_wordcounts <- blog_df %>%
  unnest_tokens(output = word, input = Text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

head(blog_wordcounts, n=20)


#News data
news_df <- tibble(Text = news) %>%
  mutate( Text = gsub(x=Text, pattern = "[0-9]+", replacement = "")) %>%
  mutate( Text = iconv(Text, from = "UTF-8", to = "ASCII//TRANSLIT"))

news_wordcounts <- news_df %>%
  unnest_tokens(output = word, input = Text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

head(news_wordcounts, n=20)

#Bigrams
twitter_bigramcounts <- twitter_df %>% 
  unnest_tokens(bigram, Text, token = "ngrams", n=2) %>%
  count(bigram, sort = TRUE)
blog_bigramcounts <- blog_df %>%
  unnest_tokens(bigram, Text, token = "ngrams", n=2) %>%
  count(bigram, sort = TRUE)
news_bigramcounts <- news_df %>%
  unnest_tokens(bigram, Text, token = "ngrams", n=2) %>%
  count(bigram, sort = TRUE)

head(twitter_bigramcounts)
head(blog_bigramcounts)
head(news_bigramcounts)

#Trigrams
twitter_trigramcounts <- twitter_df %>% 
  unnest_tokens(trigram, Text, token = "ngrams", n=3) %>%
  count(trigram, sort = TRUE)
blog_trigramcounts <- blog_df %>%
  unnest_tokens(trigram, Text, token = "ngrams", n=3) %>%
  count(trigram, sort = TRUE)
news_trigramcounts <- news_df %>%
  unnest_tokens(trigram, Text, token = "ngrams", n=3) %>%
  count(trigram, sort = TRUE)

head(twitter_trigramcounts)
head(blog_trigramcounts)
head(news_trigramcounts)

library(ggplot2)

twitter_wordcount_plot <- ggplot(data = head(twitter_wordcounts, n=10), aes(x = reorder(word, -n), y = n)) +
  #coord_cartesian(ylim = c(y_min, y_max)) +
  geom_bar(stat="identity", color = "black", fill = "slategray2") +
  labs(title = "Most Common Words on Twitter (20k Tweet Sample, Stopwords Removed)", x = "Word", y = "Count") +
  theme(plot.title = element_text(size=14, face="bold",
                                  margin = margin(10, 0, 10, 0)),
        axis.title = element_text(size=11),
        axis.text.x = element_text(size=11, vjust=0.5),
        axis.text.y = element_text(size=11, vjust=0.5),
        axis.ticks.x = element_blank()) +
  #scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
  geom_text(aes(label = n, y = n + 25),
            position = position_dodge(0.9), size=4)

twitter_wordcount_plot

blog_wordcount_plot <- ggplot(data = head(blog_wordcounts, n=10), aes(x = reorder(word, -n), y = n)) +
  #coord_cartesian(ylim = c(y_min, y_max)) +
  geom_bar(stat="identity", color = "black", fill = "slategray2") +
  labs(title = "Most Common Words in Blogs (20k Blog Sample, Stopwords Removed)", x = "Word", y = "Count") +
  theme(plot.title = element_text(size=14, face="bold",
                                  margin = margin(10, 0, 10, 0)),
        axis.title = element_text(size=11),
        axis.text.x = element_text(size=11, vjust=0.5),
        axis.text.y = element_text(size=11, vjust=0.5),
        axis.ticks.x = element_blank()) +
  #scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
  geom_text(aes(label = n, y = n + 2500),
            position = position_dodge(0.9), size=4)

blog_wordcount_plot


news_wordcount_plot <- ggplot(data = head(news_wordcounts, n=10), aes(x = reorder(word, -n), y = n)) +
  #coord_cartesian(ylim = c(y_min, y_max)) +
  geom_bar(stat="identity", color = "black", fill = "slategray2") +
  labs(title = "Most Common Words in News (20k Article Sample, Stopwords Removed)", x = "Word", y = "Count") +
  theme(plot.title = element_text(size=14, face="bold",
                                  margin = margin(10, 0, 10, 0)),
        axis.title = element_text(size=11),
        axis.text.x = element_text(size=11, vjust=0.5),
        axis.text.y = element_text(size=11, vjust=0.5),
        axis.ticks.x = element_blank()) +
  #scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
  geom_text(aes(label = n, y = n + 110),
            position = position_dodge(0.9), size=4)

news_wordcount_plot



