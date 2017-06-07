# tidytext analysis of podcast transcripts
# 2017-06-06

# load data ####

# load libraries
library(tidyverse)
library(tidytext)

# load transcript data
atp <- read_rds("data/atp.rda")
cortex <- read_rds("data/cortex.rda")
hypercritical <- read_rds("data/hypercritical.rda")
the_talk_show <- read_rds("data/tts.rda")

# clean data ####

# join podcast data together
podcasts <- bind_rows(atp, cortex, hypercritical, the_talk_show)

# get count of words by podcast
podcast_words <- podcasts %>% 
  unnest_tokens(word, line) %>%
  anti_join(stop_words) %>%
  count(podcast, word) %>%
  ungroup()

# count total words per podcast
total_words <- podcast_words %>%
  group_by(podcast) %>%
  summarise(total = sum(n))

# join individual word count and total word count
# get tf_idf by podcast
podcast_words <- left_join(podcast_words, total_words) %>% 
  bind_tf_idf(word, podcast, n)

podcast_words %>%  
  group_by(podcast) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = podcast)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~podcast, scales = "free") +
  coord_flip() +
  labs(x = "tf-idf",
       title = "Most Important Words by Podcast",
       subtitle = "Measured by tf-idf") +
  scale_fill_manual(values = c("blue4", "grey18",
                               "palegreen4", "lightsteelblue4"))+
  theme(legend.position = "none",
        axis.title.y = element_blank())

ggsave("figures/podcasts.png", height = 6, width = 10, units = "in")

podcast_plot_clean <- podcast_words %>% 
  arrange(desc(tf_idf)) %>% 
  filter(word != "mattress") %>%
  filter(word != "casper") %>%
  filter(word != "betterment") %>%
  filter(word != "lynda") %>%
  filter(word != "5x5") %>%
  filter(word != "rackspace") %>%
  filter(word != "mailchimp.com") %>%
  filter(word != "dont") %>%
  filter(word != "tts") %>%
  filter(word != "afm") %>%
  filter(word != "fd") %>%
  filter(word != "wealthfront") %>%
  filter(word != "apron") %>%
  filter(word != "cgpgrey") %>%
  mutate(word = factor(word, levels = rev(unique(word))))

podcast_plot_clean %>%  
  group_by(podcast) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = podcast)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~podcast, scales = "free") +
  coord_flip() +
  labs(x = "tf-idf",
       title = "Most Important Words by Podcast",
       subtitle = "Measured by tf-idf") +
  scale_fill_manual(values = c("blue4", "grey18",
                               "palegreen4", "lightsteelblue4"))+
  theme(legend.position = "none",
        axis.title.y = element_blank())

ggsave("figures/podcasts_clean.png", height = 6, width = 10, units = "in")

# sentiment analysis ####

podcast_sentiment <- podcast_words %>%
  select(podcast, word, n) %>%
  inner_join(get_sentiments("bing")) %>%
  ungroup() %>%
  arrange(desc(n))

podcast_sentiment %>%
  filter(podcast == "Cortex") %>%
  group_by(podcast, sentiment) %>%
  top_n(10, wt = n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
