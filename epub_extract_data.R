
library(epubr)
library(tidytext)
library(tidyverse)

# all data
df <- epub(file = "Books/Olga Ravn/De ansatte/De ansatte - Olga Ravn.epub")
desc <- df$description

# just  title and text
title <- df$title
author <- df$creator

df <- df$data[[1]]

df <- df %>% 
  mutate(title = title, author = author) %>%
  filter(nword>0,section != "title", section != "colophon", section != "tochtml",!str_detect(section,"preface")) %>%
  select(title,author,section,text)

df %>%
  unnest_tokens(word,text) %>%
  count(word,sort=T)

# get more files
# filenames <- list.files("Books", recursive = T,pattern="epub")
# path <- "C:/Users/pech/Calibre Library/"
# 
# files <- filenames %>% map_chr(~paste0(path,.))

files <- list.files("Books", recursive = T,pattern="epub")

df <- epub(paste0("books/",files),fields = c("creator","data"))

remove_sections <- c("titel|title|tochtml|preface")

datalist = list()

for (i in 1:length(df)) {
  
  author = df$creator[i]
  title = df$title[i]

  
  data = df$data[[i]]
  
  text <- data %>% 
    filter(nword>10,section != "title|colophon|tochtml",
           section != "colophon",
           section != "tochtml",!str_detect(section,remove_sections)) %>%
    select(text)
  
  datalist[[i]] <- tibble(author,title,text$text)

}

all_data <- data.table::rbindlist(datalist)

all_data <- as_tibble(all_data) %>%
  rename(text = `text$text`)

all_data %>% 
  unnest_tokens(word,text) %>%
  group_by(author) %>%
  count()
  
