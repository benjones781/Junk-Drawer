# introduction ---------------------------
# i made this little project to demonstrate that extremely simple methods can produce powerful results
# this code plots happiness/unhappiness throughout the progression of a text using EXTREMELY SIMPLE sentiment analysis
# Romeo and Juliet is used for this example
# i hope the results speak for themselves but i was pleasantly surprised with the outcome

# the lexicon used here is from the work of Bing Liu and collaborators
# inspiration for this came from Seth Stephens-Davidowitz's book, 'Everybody Lies'


# setup ---------------------------
library(rvest)
library(stringr)
library(tidytext)
library(dplyr)
library(ggplot2)

get_page_text <- function(page) {
    read_html(page) %>%
    html_nodes("p") %>%
    html_text2()
}

clean_page_text <- function(page_text){
  page_text %>%
  tolower() %>%
  trimws() %>%
  str_replace_all(pattern = c("\\n" = " ")) %>%
  gsub(pattern = "[[:punct:][:blank:]]+", replacement =  " ",) %>%
  str_squish()
}

convert_text_to_df <- function(text) {
  df <- 
    text %>%
    strsplit(split = " ") %>%
    as.list() %>%
    as.data.frame()
  
  colnames(df) <- "word"
  
  return(df)
}

full_text <- paste(c(get_page_text("https://www.fulltextarchive.com/page/Romeo-and-Juliet1/"),
                     get_page_text("https://www.fulltextarchive.com/page/Romeo-and-Juliet2/"),
                     get_page_text("https://www.fulltextarchive.com/page/Romeo-and-Juliet3/")),
                   collapse = " ") # full script is divided across multiple urls

full_text_df <-
  full_text %>%
  clean_page_text() %>%
  convert_text_to_df()


# sentiment analysis ---------------------------
bing_df <- left_join(full_text_df, get_sentiments("bing"))

bing_df$timeline <- 1/nrow(bing_df)*100 # to get timeline as x-axis

bing_df <- 
  bing_df %>%
  mutate(score = case_when(
    sentiment == "negative" ~ -1,
    is.na(sentiment)  ~ 0,
    sentiment == "positive" ~ 1,)
    )

bing_df <- 
  bing_df %>%
  mutate(cumulative_score = cumsum(score)) %>%
  mutate(timeline = cumsum(timeline))


# visualizations ---------------------------
# unmarked and unannotated
ggplot(bing_df, aes(x=timeline, y=cumulative_score)) +
  geom_line(color = "dodgerblue") +
  labs(title = "Star-Crossed Lovers",
       subtitle = "sentiment progression throughout Romeo and Juliet",
       y = "sentiment",
       x = "story progression")

# marked and unannotated
ggplot(bing_df, aes(x=timeline, y=cumulative_score)) +
  geom_line(color = "dodgerblue") +
  labs(title = "Star-Crossed Lovers",
       subtitle = "unidentified key moments (can you guess what they are?)",
       y = "sentiment",
       x = "story progression") +
  geom_point(data = bing_df[c(1250, 7259, 12435, 15843, 22247, 23120), ],
             aes(x = timeline, y = cumulative_score),
             color = "red",
             shape = 21,
             size = 8)

# marked and annotated
ggplot(bing_df, aes(x=timeline, y=cumulative_score)) +
  geom_line(color = "dodgerblue") +
  labs(title = "Star-Crossed Lovers",
       subtitle = "key moments identified",
       y = "sentiment",
       x = "story progression") +
  geom_point(data = bing_df[c(1250, 7259, 12435, 15843, 22247, 23120), ],
             aes(x = timeline, y = cumulative_score),
             color = "red",
             shape = 21,
             size = 8) +
  annotate("text", x = 5, y = -55, size = 1.75, label = "Romeo is depressed about his \nunrequited love for Rosaline") +
  annotate("text", x = 30, y = -5, size = 1.75, label = '"O Romeo, Romeo! \nwherefore art thou Romeo?"') +
  annotate("text", x = 40, y = 120, size = 1.75, label = "Romeo and Juliet \nmarry in secret") +
  annotate("text", x = 67, y = 25, size = 1.75, label = "Mercutio and Tybalt die, \nRomeo is banished from Verona") +
  annotate("text", x = 77, y = -80, size = 1.75, label = "Capulets believe \nJuliet is dead") +
  annotate("text", x = 90, y = -35, size = 1.75, label = "Balthasar tells Romeo \nJuliet is dead")


# sentiment analysis with weighted sentiments --------------------
afinn_df <- left_join(full_text_df, get_sentiments("afinn"))

afinn_df$timeline <- 1/nrow(afinn_df)*100 # to get timeline as x-axis

afinn_df <- 
  afinn_df %>%
  mutate(value = case_when(
    is.na(value)  ~ 0,
    !is.na(value) ~ value)
  )
afinn_df <- 
  afinn_df %>%
  mutate(cumulative_score = cumsum(value)) %>%
  mutate(timeline = cumsum(timeline))

ggplot(afinn_df, aes(x=timeline, y=cumulative_score)) +
  geom_line(color = "dodgerblue") +
  labs(title = "Star-Crossed Lovers",
       subtitle = "weighted sentiment progression throughout Romeo and Juliet",
       y = "sentiment",
       x = "story progression")

