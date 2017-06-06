# transcripts scraped on 2017-06-06

library(tidyverse)
library(RCurl)
library(rvest )
library(stringr)
library(tidytext)

# cortex ####

# get cortext show links
cortex_url <- getURL("http://podsearch.david-smith.org/shows/5")
cortex_page <- readLines(tc <- textConnection(cortex_url)); close(tc)

cortex_df <- tibble(line = 1:length(cortex_page), content =cortex_page)

cortex_links <- cortex_df %>%
  filter(str_detect(content, "episodes/[0-9]*")) %>%
  mutate(ep_title = str_extract_all(content, "Cortex [0-9]+: .*"),
         ep_title = str_replace_all(ep_title, "</a></h4>", ""), 
         ep_num = str_extract_all(ep_title, "Cortex [0-9]+"),
         ep_num = as.numeric(str_replace_all(ep_num, "Cortex ", "")),
         ep_title = str_replace_all(ep_title, "Cortex [0-9]+: ", ""))%>%
  mutate(url_end = str_extract_all(content, "episodes/[0-9]+"),
         url = str_c("http://podsearch.david-smith.org/", url_end),
         podcast = "Cortex") %>%
  select(podcast, ep_num, ep_title, url) %>%
  arrange(ep_num)

cortex_transcripts <- tibble(ep_num = integer(), line_num = integer(),
                             line = character())

for(i in seq_along(1:length(cortex_links$url))){
  
  episode_url <- cortex_links$url[i]
  
  episode_data <- read_html(episode_url) %>%
    html_nodes("div")
  
  episode_html <- as.character(episode_data[1][1])
  
  full_text <- episode_html %>%
    str_extract_all("►</a> \n    [a-zA-Z0-9[:punct:] ]+")
  
  ep_tbl <- tibble(line_num =  1:length(full_text[[1]]), 
                   raw_line = full_text[[1]]) %>%
    mutate(line = str_replace_all(raw_line,"►</a> \n    ","" ),
           ep_num = i) %>%
    select(ep_num, line_num, line) 
  
  cortex_transcripts <- bind_rows(cortex_transcripts, ep_tbl)
}

cortex_data <- cortex_links %>%
  select(-url) %>%
  full_join(cortex_transcripts)

saveRDS(cortex_data, "data/cortex.rda")

# atp ####

# get atp show links
atp_url <- getURL("http://podsearch.david-smith.org/shows/2")
atp_page <- readLines(tc <- textConnection(atp_url)); close(tc)

atp_df <- tibble(line = 1:length(atp_page), content = atp_page)

atp_links <- atp_df %>%
  filter(str_detect(content, "episodes/[0-9]*")) %>%
  mutate(ep_title = str_extract_all(content, "[0-9]+: .*")) %>%
  separate(ep_title, c("ep_num", "ep_title"), sep = ": ") %>%
  mutate(url_end = str_extract_all(content, "episodes/[0-9]+"),
         url = str_c("http://podsearch.david-smith.org/", url_end),
         ep_title = str_replace_all(ep_title, "</a></h4>", ""),
         ep_num = as.numeric(ep_num),
         podcast = "ATP") %>%
  select(podcast, ep_num, ep_title, url) %>%
  arrange(ep_num)


atp_transcripts <- tibble(ep_num = integer(), line_num = integer(),
                             line = character())

for(j in seq_along(1:length(atp_links$url))){
  
  episode_url <- atp_links$url[j]
  
  episode_data <- read_html(episode_url) %>%
    html_nodes("div")
  
  episode_html <- as.character(episode_data[1][1])
  
  full_text <- episode_html %>%
    str_extract_all("►</a> \n    [a-zA-Z0-9[:punct:] ]+")
  
  ep_tbl <- tibble(line_num =  1:length(full_text[[1]]), 
                   raw_line = full_text[[1]]) %>%
    mutate(line = str_replace_all(raw_line,"►</a> \n    ","" ),
           ep_num = j) %>%
    select(ep_num, line_num, line) 
  
  atp_transcripts <- bind_rows(atp_transcripts, ep_tbl)
}

atp_data <- atp_links %>%
  select(-url) %>%
  full_join(atp_transcripts)

saveRDS(atp_data, "data/atp.rda")



# hypercritical ####

# get hi show links
hypercritical_url <- getURL("http://podsearch.david-smith.org/shows/8")
hypercritical_page <- readLines(tc <- textConnection(hypercritical_url)); close(tc)

hypercritical_df <- tibble(line = 1:length(hypercritical_page),
                           content = hypercritical_page)

hypercritical_links <- hypercritical_df %>%
  filter(str_detect(content, "episodes/[0-9]*")) %>%
  mutate(ep_title = str_extract_all(content, "[0-9]+: .*")) %>%
  separate(ep_title, c("ep_num", "ep_title"), sep = ": ") %>%
  mutate(url_end = str_extract_all(content, "episodes/[0-9]+"),
         url = str_c("http://podsearch.david-smith.org/", url_end),
         ep_title = str_replace_all(ep_title, "</a></h4>", ""),
         ep_num = as.numeric(ep_num),
         podcast = "Hypercritical") %>%
  select(podcast, ep_num, ep_title, url) %>%
  arrange(ep_num)


hypercritical_transcripts <- tibble(ep_num = integer(), line_num = integer(),
                          line = character())

for(k in seq_along(1:length(hypercritical_links$url))){
  
  episode_url <- hypercritical_links$url[k]
  
  episode_data <- read_html(episode_url) %>%
    html_nodes("div")
  
  episode_html <- as.character(episode_data[1][1])
  
  full_text <- episode_html %>%
    str_extract_all("►</a> \n    [a-zA-Z0-9[:punct:] ]+")
  
  ep_tbl <- tibble(line_num =  1:length(full_text[[1]]), 
                   raw_line = full_text[[1]]) %>%
    mutate(line = str_replace_all(raw_line,"►</a> \n    ","" ),
           ep_num = k) %>%
    select(ep_num, line_num, line) 
  
  hypercritical_transcripts <- bind_rows(hypercritical_transcripts, ep_tbl)
}

hypercritical_data <- hypercritical_links %>%
  select(-url) %>%
  full_join(hypercritical_transcripts)

saveRDS(hypercritical_data, "data/hypercritical.rda")


# the talk show ####

# get hi show links
tts_url <- getURL("http://podsearch.david-smith.org/shows/3")
tts_page <- readLines(tc <- textConnection(tts_url)); close(tc)

tts_df <- tibble(line = 1:length(tts_page),
                           content = tts_page)

tts_links <- tts_df %>%
  filter(str_detect(content, "episodes/[0-9]*")) %>%
  mutate(ep_title = str_extract_all(content, "[A-Z]*[0-9]*: .*"),
         ep_title = str_replace(ep_title, "XCIX", "99"),
         ep_num = str_extract(ep_title, "[0-9]+:"),
         ep_num = as.numeric(str_replace_all(ep_num, ":", "")), 
         ep_title = str_replace(ep_title, "[0-9]+: ", "")) %>%
  mutate(url_end = str_extract_all(content, "episodes/[0-9]+"),
         url = str_c("http://podsearch.david-smith.org/", url_end),
         ep_title = str_replace_all(ep_title, "</a></h4>", ""),
         ep_num = as.numeric(ep_num),
         podcast = "The Talk Show") %>%
  select(podcast, ep_num, ep_title, url) %>%
  arrange(ep_num)


tts_transcripts <- tibble(ep_num = integer(), line_num = integer(),
                                    line = character())

for(l in seq_along(1:length(tts_links$url))){
  
  episode_url <- tts_links$url[l]
  
  episode_data <- read_html(episode_url) %>%
    html_nodes("div")
  
  episode_html <- as.character(episode_data[1][1])
  
  full_text <- episode_html %>%
    str_extract_all("►</a> \n    [a-zA-Z0-9[:punct:] ]+")
  
  ep_tbl <- tibble(line_num =  1:length(full_text[[1]]), 
                   raw_line = full_text[[1]]) %>%
    mutate(line = str_replace_all(raw_line,"►</a> \n    ","" ),
           ep_num = l) %>%
    select(ep_num, line_num, line) 
  
  tts_transcripts <- bind_rows(tts_transcripts, ep_tbl)
  
  print(paste0("Episode ", l, " scraping complete."))
}

tts_data <- tts_links %>%
  select(-url) %>%
  full_join(tts_transcripts)

saveRDS(hypercritical_data, "data/tts.rda")
