# analysis of atp transcripts
# 2017-06-06

# load data ####

# load libraries
library(tidyverse)
library(tidytext)
library(stringr)
library(igraph)
library(ggraph)

# load transcript data
atp <- read_rds("data/atp.rda")

# clean data ####

# get count of words by episode
atp_words <- atp %>% 
  unnest_tokens(word, line) %>%
  anti_join(stop_words) %>%
  count(ep_num, ep_title, word) %>%
  ungroup()

# count total words per podcast
total_words <- atp_words %>%
  group_by(ep_num) %>%
  summarise(total = sum(n))

# join individual word count and total word count
# get tf_idf by podcast
atp_words <- left_join(atp_words, total_words) %>% 
  bind_tf_idf(word, ep_num, n)

atp_word_plot <- atp_words %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(word = factor(word, levels = rev(unique(word))),
         ep_title = factor(ep_title, levels = ep_title))

atp_word_plot %>% 
  filter(ep_num >170) %>%
  group_by(ep_num) %>%
  top_n(10) %>% 
  ungroup() %>% 
  ggplot(aes(x = word, y=tf_idf)) +
  geom_col(show.legend=FALSE) +
  coord_flip() +
  labs(x = "tf-idf",
       title = "Most Important Words by atp Episode",
       subtitle = "Measured by tf-idf") +
  facet_wrap(~ep_title, scales = "free_y")+
  theme(legend.position = "none",
        axis.title.y = element_blank())

# sentiment analysis ####

atp_ep <- atp %>%
  group_by(ep_num, ep_title) %>%
  unnest_tokens(word, line) %>%
  ungroup() %>%
  select(-line_num)

bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

bingpositive <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")

atp_ep_wordcounts <- atp_ep %>%
  group_by(ep_num, ep_title) %>%
  summarise(words = n())

atp_negative <- atp_ep %>%
  semi_join(bingnegative) %>%
  group_by(ep_num, ep_title) %>%
  summarize(negativewords = n()) %>%
  left_join(atp_ep_wordcounts, by = c("ep_num", "ep_title")) %>%
  ungroup() %>%
  mutate(ratio = negativewords/words,
         relative_neg = mean(ratio) - ratio) %>%
  ungroup()

atp_positive <- atp_ep %>%
  semi_join(bingpositive) %>%
  group_by(ep_num, ep_title) %>%
  summarize(positivewords = n()) %>%
  left_join(atp_ep_wordcounts, by = c("ep_num", "ep_title")) %>%
  ungroup() %>%
  mutate(ratio = positivewords/words,
         relative_pos = mean(ratio) - ratio) %>%
  ungroup()

atp_pos_neg <- atp_positive %>%
  select(-ratio, -relative_pos) %>%
  left_join(atp_negative) %>%
  select(-ratio, -relative_neg) %>%
  mutate(net_positive = positivewords - negativewords,
         net_ratio = net_positive / words,
         relative_ratio = net_ratio - mean(net_ratio) )

ggplot(atp_negative, aes(x = ep_num, y = relative_neg))+
  geom_bar(stat = "identity")+
  geom_smooth(span = .3, se = FALSE, color = "grey64")

ggplot(atp_positive, aes(x = ep_num, y = relative_pos))+
  geom_bar(stat = "identity")+
  geom_smooth(span = .3, se = FALSE, color = "grey64")

ggplot(atp_pos_neg, aes(x = ep_num, y = relative_ratio))+
  geom_bar(stat = "identity")+
  geom_smooth(span = .1, se = FALSE, color = "royalblue4")

# ngrams ####

atp_bigrams <- atp %>%
  group_by(ep_num, ep_title) %>%
  summarise(text = paste(line, collapse = "")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word1 != "yeah" & word2 != "yeah") %>%
  filter(word1 != "code" & word2 != "atp") %>% 
  filter(word1 != "blue" & word2 != "apron") %>%
  filter(word1 != "ten" & word2 != "percent") %>%
  unite(bigram, word1, word2, sep = " ") %>%
  mutate(ep_title = factor(ep_title, levels = atp_ep_wordcounts$ep_title)) %>%
  count(ep_title, bigram, sort = TRUE)

total_bigrams <- atp_bigrams %>%
  group_by(ep_title) %>%
  summarise(total = sum(n))

# join individual word count and total word count
# get tf_idf by podcast
atp_bigram_tf_idf <- left_join(atp_bigrams, total_bigrams) %>% 
  bind_tf_idf(bigram, ep_title, n)

atp_bigram_plot <- atp_bigram_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(bigram = factor(bigram, levels = rev(unique(bigram))))

atp_bigram_plot %>%  
  group_by(ep_title) %>%
  top_n(10) %>% 
  ungroup() %>% 
  ggplot(aes(x = bigram, y=tf_idf)) +
  geom_col(show.legend=FALSE) +
  coord_flip() +
  labs(x = "tf-idf",
       title = "Most Important Bigrams by atp Episode",
       subtitle = "Measured by tf-idf") +
  facet_wrap(~ep_title, scales = "free_y")+
  theme(legend.position = "none",
        axis.title.y = element_blank())

atp_bigrams <- atp %>%
  group_by(ep_num, ep_title) %>%
  summarise(text = paste(line, collapse = "")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word1 != "yeah" & word2 != "yeah") %>%
  filter(word1 != "code" & word2 != "atp") %>% 
  filter(word1 != "blue" & word2 != "apron") %>%
  filter(word1 != "ten" & word2 != "percent") %>%
  filter(word1 != "credit" & word2 != "card") %>%
  filter(word1 != "card" & word2 != "required") %>%
  filter(word1 != "real" & word2 != "afm") %>%
  filter(word1 != "text" & word2 != "expander") %>%
  filter(word1 != "pdf" & word2 != "pen") %>%
  filter(word1 != "ten" & word2 != "percent") %>%
  filter(word1 != "30" & word2 != "day") %>%
  filter(word1 != "offer" & word2 != "code") %>%
  filter(word1 != "fresh" & word2 != "books") %>%
  filter(word1 != "free" & word2 != "trial") %>%
  filter(word1 != "uni" & word2 != "box") %>%
  filter(word1 != "risk" & word2 != "free") %>%
  filter(word1 != "completely" & word2 != "risk") %>%
  filter(word1 != "movement" & word2 != "watches") %>%
  filter(word1 != "incredible" & word2 != "home") %>%
  filter(word1 != "home" & word2 != "cooked") %>% 
  filter(word1 != "cooked" & word2 != "meals") %>%
  filter(word1 != "super" & word2 != "fast") %>%
  filter(word1 != "super" & word2 != "easy") %>%
  filter(word1 != "super" & word2 != "simple") %>%
  filter(word1 != "freshbooks" & word2 != "customers") %>%
  filter(word1 != "conditions" & word2 != "apply") %>%
  filter(word1 != "professionally" & word2 != "designed") %>%
  filter(word1 != "start" & word2 != "building") %>%
  filter(word1 != "live" & word2 != "chat") %>%
  filter(word1 != "continued" & word2 != "support") %>%
  filter(word1 != "fifty" & word2 != "dollars") %>%
  filter(word1 != "hundred" & word2 != "dollars") %>%
  filter(word1 != "perfect" & word2 != "domain") %>%
  filter(word1 != "domain" & word2 != "dames") %>%
  filter(word1 != "final" & word2 != "sponsor") %>%
  filter(word2 != "minutes") %>%
  filter(word1 != "10" & word2 != "people") %>%
  filter(word1 != "week's" & word2 != "episode") %>%
  filter(word1 != "today's" & word2 != "episode") %>%
  filter(word1 != "highly" & word2 != "recommend") %>%
  filter(word1 != "commerce" & word2 != "platform") %>%
  filter(word1 != "razor" & word2 != "blade") %>%
  filter(word1 != "razor" & word2 != "blades") %>%
  filter(word1 != "15" & word2 != "bucks") %>%
  filter(word1 != "20" & word2 != "bucks") %>%
  filter(word1 != "50" & word2 != "bucks") %>%
  filter(word1 != "mail" & word2 != "route") %>%
  filter(word1 != "mail" & word2 != "server") %>%
  filter(word1 != "slash" & word2 != "talk") %>%
  filter(word1 != "memory" & word2 != "foam") %>%
  filter(word1 != "squarespace" & word2 != "dot") %>%
  filter(word1 != "backblaze" & word2 != "dot") %>%
  filter(word1 != "software" & word2 != "dot") %>%
  filter(word1 != "fracture" & word2 != "me.com") %>%
  filter(word1 != "base" & word2 != "camp") %>%
  filter(word1 != "square" & word2 != "space") %>%
  filter(word1 != "warby" & word2 != "parker") %>%
  filter(word1 != "online" & word2 != "backup") %>%
  count(word1, word2, sort = TRUE) 

bigram_graph <- atp_bigrams %>% 
  filter(n > 100) %>% 
  graph_from_data_frame()



set.seed(12)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), 
                 show.legend = FALSE,
                 arrow = a, end_cap = circle(.03, 'inches')) +
  geom_node_point(how.legend = FALSE) +
  geom_node_point(aes(size = n), color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

 ggsave("figures/atp_bigrams.png", height = 8, width = 8, units = "in")
