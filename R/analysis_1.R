# load libraries
library(rmarkdown)
library(knitr)
library(tidyverse)
library(tidyr)
library(reshape2)
library(tidytext)
library(magrittr)
library(plyr)
library(reshape2)
library(dplyr)
library(yelpr)
library(qpcR)
library(curl)
library(httr)
library(jsonlite)
library(RCurl)
library(magrittr)
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(broom)
library(textdata)
library(wordcloud)
library(wordcloud2)
library(igraph)
library(ggraph)
library(networkD3)

# define tokens
client_id <- "SQ3SyM_wbzFDMvSqxv9K6A"
client_secret <- "GpwHQcQIC5Z3fCZIWhxc5BOTTpUrhI2QZ_JF5ZAWI9duiOIy7xLWp8zguUHnLpgYt7HAONZ1Hw56nWYrN-0JBenp2e4Rb37IVsy8gd4D5Gf6s4Cbi8DuscO0LeAQYHYx"

### Yelp Fusion API: Analysis 1 ###
# Step 1
# Parameters:
term <- c("coffee")
location <- "New York, NY"
limit <- 50
offset <- seq(0, 950, 50)

#Test authentification:
(url <-
    modify_url("https://api.yelp.com", path = c("v3", "businesses", "search"),
               query = list(term = term,
                            location = location, limit = limit)))
res <- GET(url, add_headers('Authorization' = paste("Bearer", client_secret)))

#checkstatus:
http_status(res)

# Step 2
# Loop to pull max amount of data for specified business: "coffee"
yelp_fusion <- "https://api.yelp.com"
yf <- data.frame()
for(i in 1:20) {
  (url <- modify_url(
    yelp_fusion,
    path = c("v3", "businesses", "search"),
    query = list(
      term = term,
      location = location,
      limit = limit,
      offset = offset[i]
    )
  ))
  
  l = GET(url, add_headers('Authorization' = paste("Bearer", client_secret)))
  m = content(l)
  n = jsonlite::fromJSON(toJSON(m))
  
  yf_output = tryCatch({
    data.frame(n)
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(yf_output)) {
    yf <-
      rbind(
        yf,
        data.frame(
          'id' = unlist(yf_output$businesses.id),
          'name' = unlist(yf_output$businesses.name),
          'price' = unlist(as.character(yf_output$businesses.price)),
          'rating' = unlist(yf_output$businesses.rating),
          'review_count' = unlist(yf_output$businesses.review_count),
          'city' = unlist(yf_output$businesses.location$city),
          'state' = unlist(yf_output$businesses.location$state),
          'zip_code' = unlist(yf_output$businesses.location$zip_code),
          'url' = unlist(yf_output$businesses.url),
          'latitude' = unlist(yf_output$businesses.coordinates$latitude),
          'longitude' = unlist(yf_output$businesses.coordinates$longitude)
        )
      )
  }
}

yf <- yf %>% 
  distinct(name, .keep_all = TRUE) %>% 
  arrange(desc(rating))
yf

# Step 3
# Plot Yelp ratings vs. review counts
ggplot(yf, aes(x = factor(rating) , y = review_count)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitter(height = 0, width = 0.25), shape = 1, alpha = 0.4, color = "blue") +
  labs(title="Yelp: Rating vs Review Count for New York Coffee Shops") +
  labs(x="Rating", y="Review Count") +
  geom_hline(yintercept = 0, size = 1, color = "darkgreen") +
  theme_minimal()

# Step 4
# Plot Yelp prices vs. review counts
ggplot(yf, aes(x = factor(price), y = review_count)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitter(height = 0, width = 0.25), shape = 1, alpha = 0.4, color = "purple") +
  labs(title="Yelp: Price vs Review Count for New York Coffee Shops") +
  labs(x="Price", y="Review Count") +
  geom_hline(yintercept = 0, size = 1, color = "darkgreen") +
  theme_minimal()

# Step 5
# Plot Yelp prices vs. ratings
ggplot(yf, aes(x = factor(price) , y = rating)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitter(height = 0, width = 0.25), shape = 1, alpha = 0.4, color = "red") +
  labs(title="Yelp: Price vs Rating for New York Coffee Shops") +
  labs(x="Price", y="Rating") +
  geom_hline(yintercept = 0, size = 1, color = "darkgreen") +
  theme_minimal()

# Step 6
# Plot map locations vs. ratings
ggplot(subset(yf, city %in% c("Brooklyn", "New York", "Staten Island", "Queens", "Bronx")),
       aes(x = factor(city), y = rating)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitter(height = 0, width = 0.25), shape = 1, alpha = 0.4, color = "orange") +
  labs(title="Yelp: Price vs Rating for New York Coffee Shops") +
  labs(x="city", y="Rating") +
  geom_hline(yintercept = 0, size = 1, color = "darkgreen") +
  theme_minimal()


### Yelp Fusion API: Analysis 2, Text Analysis ###
# Step 1
# Loop extracts the ID from Yelp Fusion
id_yf <- yf %>% 
  select_(id = "id")

x <- nrow(yf)

id_list <- capture.output({for (i in 1:x){
  id <- as.character(id_yf$id)[i]
  root <- "https://api.yelp.com"
  u <- paste(root, "/v3", "/businesses/", id, "/reviews", sep = "")
  if (u=="") {
    warning("error")}
  else {
    print(u)
  }}})

#Format list:
#extract url

url_list1 <- str_replace(id_list, "^[[:punct:]][1][[:punct:]][[:space:]][[:punct:]]", "")

final_url_list <- str_replace(url_list1, "[[:punct:]]$", "")

# Step 2
# Loop for full list of reviews from Yelp Fusion
f <- data.frame()  
for(i in 1:x){
  url2 <- final_url_list[i]
  if(url2=="") {
    warning("error") }
  else {
    a <- GET(url2, add_headers('Authorization' = paste("Bearer", client_secret)))
    b <- content(a)
    c <- jsonlite::fromJSON(toJSON(b))
    
    d = tryCatch({
      data.frame(b)
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(d)) {
      f <-
        rbind(
          f,
          data.frame(
            'user_name' = unlist(d$reviews.user.name),
            'text' = unlist(d$reviews.text),
            'rating' = unlist(d$reviews.rating),
            'url' = unlist(as.character(d$reviews.url)),
            'id' = unlist(d$reviews.id),
            'user_id' = unlist(d$reviews.user.id)
          )
        )
    }
  }}

f <- f %>% 
  distinct(name, .keep_all = TRUE)
f

# Step 3
# Transform data columns to appropriate format to allow merger
# of Yelp Fusion Search and Review datasets

#extract id

id3 <- stri_match_first_regex(f$url, "(.*?)\\?")[,2]

id2 <- substring(id3, regexpr("z/", id3) + 1)

id1 <- substring(id2, regexpr("/", id2) + 1)

#add as a column to f.db

yelp_search <- yf
yelp_review <- f
yelp_review$id <- id1
#f$id<- id1

# Splitting URL to two, separated by "?"
yelp_search <- separate(yelp_search, col = "url", into = c("url_cleaned", "rubbish"), sep = "[?]")
yelp_review <- separate(yelp_review, col = "url", into = c("url_cleaned", "rubbish"), sep = "[?]")

# Step 4
# Merge datasets for Yelp Fusion Search and Review
Yelp_full <- inner_join(yelp_review, yelp_search, by = "url_cleaned")

# Rename columns and clean up


Yelp_full

# Step 5
# Text mining using tidytext
# Plot shows count associated with each word
text1 <- Yelp_full %>%
  select_(text = "text")

tn <- nrow(text1)

class(text1)

text2 <- data.frame(lapply(text1, as.character), stringsAsFactors=FALSE)

text_df <- data.frame(line = 1:tn, text2)
text_df

##tidytext stop words
data(stop_words)

plot_tcount <- text_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by=c("word"="word")) %>%
  count("word", wt_var = NULL) %>%
  filter(freq > 20) %>%
  arrange(desc(freq)) %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

plot_tcount

################START################

# Generating count of words
text_df %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% c("coffee", "shop", "covid", "19", "cafe", "pandemic", "time")) %>%
  anti_join(stop_words) %>%
  count("word") %>%
  filter(freq > 20) %>%
  arrange(desc(freq)) %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word, freq)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Generating wordcloud2
text_df %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% c("coffee", "shop", "covid", "19", "2", "5", "10", "20", "cafe", "pandemic", "time")) %>%
  anti_join(stop_words) %>%
  count("word") %>%
  wordcloud2(color = "random-dark",
             minRotation = -pi/4,
             maxRotation = pi/4,
             rotateRatio = 0.4,
             size = 0.8,
             shape = "circle",
             ellipticity = 0.65,
             backgroundColor = "white")

# Generating wordcloud
text_df %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% c("coffee", "shop", "covid", "19", "cafe", "pandemic", "time")) %>%
  anti_join(stop_words) %>%
  count("word") %>%
  with(wordcloud(word, 
                 freq, 
                 min.freq=1, 
                 max.words=500, 
                 random.order=F, 
                 rot.per=0.35, 
                 colors=brewer.pal(8,"Dark2"),
                 scale=c(3.5,0.25)))

text_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count("word") %>%
  with(wordcloud(word, freq, min.freq=2, max.words=200, random.order=F, rot.per=0.35, colors=brewer.pal(8,"Dark2"), scale=c(3.5,0.25)))

# Generating comparison cloud
bing_new <- get_sentiments("bing") %>%
  mutate(sentiment = replace(sentiment, word=="cold", "positive"))

text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(bing_new) %>%
  dplyr::count(word, sentiment, sort = T) %>%
  acast(word ~ sentiment, value.var="n", fill=0) %>%
  comparison.cloud(random.order=F, colors=c("tomato1", "aquamarine3"),
                   max.words=500,
                   title.size=1.5)

# Relationships between words: n-grams and correlations
text_df_bigrams <- text_df %>%
  unnest_tokens(bigram, text, token="ngrams", n=2) %>%
  dplyr::count(bigram, sort=T)

bigrams_separated <- text_df_bigrams %>%
  separate(bigram, c("word1", "word2"), sep=" ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>%
  dplyr::count(word1, word2, sort=T)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep= " ")

bigrams_separated %>%
  filter(word1 == "not") %>%
  dplyr::count(word1, word2, sort=T)

AFINN <- get_sentiments("afinn")
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by=c(word2="word")) %>%
  dplyr::count(word2, value, sort=T)

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = F) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

negation_words <- c("not", "no", "never", "without")
negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  dplyr::count(word1, word2, value, sort=T)

negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = F) + 
  facet_wrap(~word1) +
  labs(x = "Sentiment value * # of occurrences",
       y = "Words preceded by \"not\"")

bigrams_filtered %>%
  filter(!word1 %in% c("coffee", "covid"),
         !word2 %in% c("covid")) %>%
  filter(n > 2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), repel = T,
                 point.padding = unit(0.2, "lines")) +
  theme_graph() +
  theme(legend.position = "none")

# Interactive network chart
bigrams_interactive %>%
  simpleNetwork(height = "100px", width = "100px",
                Source = 1,
                Target = 2,
                linkDistance = 0.1,
                charge = -900,
                fontSize = 14,
                fontFamily = "serif",
                linkColour = "#666",
                nodeColour = "69b3a2",
                opacity = 0.9,
                zoom = T)

################END################

# Step 6
# Text mining techniques using NCR
#learn NCR sentiments and sum words from Yelp reviews per sentiment
ncr <- get_sentiments("nrc")
ncr_sent_groupings <- unique(ncr$sentiment, incomparables = FALSE)
ncr_sent_groupings

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
nrc_fear <- get_sentiments("nrc") %>% 
  filter(sentiment == "fear")
nrc_negative <- get_sentiments("nrc") %>% 
  filter(sentiment == "negative")
nrc_sadness <- get_sentiments("nrc") %>% 
  filter(sentiment == "sadness")
nrc_anger <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger")
nrc_surprise <- get_sentiments("nrc") %>% 
  filter(sentiment == "surprise")
nrc_positive <- get_sentiments("nrc") %>% 
  filter(sentiment == "positive")
nrc_disgust <- get_sentiments("nrc") %>% 
  filter(sentiment == "disgust")
nrc_anticipation <- get_sentiments("nrc") %>% 
  filter(sentiment == "anticipation")
nrc_trust <- get_sentiments("nrc") %>% 
  filter(sentiment == "trust")

ncr_sent_joy <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc_joy) %>%
  count("word", wt_var = NULL)
joy <- sum(ncr_sent_joy$freq)
joy

ncr_sent_fear <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc_fear) %>%
  count("word", wt_var = NULL)
fear <- sum(ncr_sent_fear$freq)
fear

ncr_sent_negative <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc_negative) %>%
  count("word", wt_var = NULL)
negative <- sum(ncr_sent_negative$freq)
negative

ncr_sent_sadness <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc_sadness) %>%
  count("word", wt_var = NULL)
sadness <- sum(ncr_sent_sadness$freq)
sadness

ncr_sent_anger <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc_anger) %>%
  count("word", wt_var = NULL)
anger <- sum(ncr_sent_anger$freq)
anger

ncr_sent_surprise <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc_surprise) %>%
  count("word", wt_var = NULL)
surprise <- sum(ncr_sent_surprise$freq)
surprise

ncr_sent_positive <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc_positive) %>%
  count("word", wt_var = NULL)
positive <- sum(ncr_sent_positive$freq)
positive

ncr_sent_disgust <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc_disgust) %>%
  count("word", wt_var = NULL)
disgust <- sum(ncr_sent_disgust$freq)
disgust

ncr_sent_anticipation <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc_anticipation) %>%
  count("word", wt_var = NULL)
anticipation <- sum(ncr_sent_anticipation$freq)
anticipation

ncr_sent_trust <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc_trust) %>%
  count("word", wt_var = NULL)
trust <- sum(ncr_sent_trust$freq)
trust

sentiment_scores <- as.data.frame(cbind(trust, anger, anticipation, disgust, fear, joy, negative, positive, sadness, surprise))

sentiment_scores.df <- melt(sentiment_scores)
sentiment_scores.df <- sentiment_scores.df %>%
  arrange(desc(value))

sentiment_scores.df

##arrange bars in order

sentiment_scores.df$variable <- factor(sentiment_scores.df$variable, levels = sentiment_scores.df$variable[order(sentiment_scores.df$value)])
sentiment_scores.df$variable

plot_scount <- sentiment_scores.df %>%
  ggplot(aes(variable, value, fill=variable)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  geom_bar(stat = "identity")

plot_scount

# Step 7
# Text mining using Bing
bing <- get_sentiments("bing")

bing_sentiment <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"), by=c("word"="word")) %>%
  count("sentiment", wt_var = NULL)

net_sentiment <- bing_sentiment %>%
  mutate(netsentiment = freq - first(freq))

net_sentiment1 <- net_sentiment$netsentiment

net_sentiment2 <- net_sentiment1[2]

bing_sentiment_scores <- as.data.frame(rbind(bing_sentiment, net_sentiment2))

bing_sentiment_scores$sentiment <- with(bing_sentiment_scores, ifelse(sentiment=="1338", "net sentiment", sentiment))
bing_sentiment_scores

##plot

plot_bcount <- bing_sentiment_scores %>%
  ggplot(aes(sentiment, freq, fill=sentiment)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  geom_bar(stat = "identity")

plot_bcount
