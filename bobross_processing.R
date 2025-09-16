library(tidyverse)
library(stringr)
library(rvest)
library(xml2)

home_dir <- path.expand("~")
picasso_path <- file.path(home_dir, "Library/CloudStorage/Box-Box/QuantifyingPicasso")
bob_ross_path <- file.path(picasso_path, "bob_ross")

URL <- "https://en.wikipedia.org/wiki/List_of_The_Joy_of_Painting_episodes"
pg <- read_html(URL)
tbls <- html_nodes(pg, "table") # extract all tables from the page

# get wikiepisodetable from the wikipedia page
ep_tbls <- html_table(tbls[grep("wikiepisodetable",tbls,ignore.case = T)],fill = T)

complete_ep_df <- NULL
 
for(i in 1:length(ep_tbls)) {
  df <- ep_tbls[[i]]
  if (ncol(df) == 4) {
    names(df) <- c('no.overall', 'no.season', 'title', 'release_date')
  } else {
    names(df) <- c('no.overall', 'no.season', 'title', 'release_date', 'none')
  }
  
  # remove anomalous rows often describing a special guest
  rm_inds <- which(is.na(as.numeric(df$no.overall)))
  if(length(rm_inds) > 0) {
    df <- df[-rm_inds,]
  }
  
  df$season <- i
  df$title <- gsub("\"", "", df$title)
  
  # extract dates for Bob Ross episode
  df <- df %>% 
    mutate(rdate = gsub("^.*\\(", "", release_date),
           release_date = substr(rdate, 1, nchar(rdate)-1)) %>% 
    select(season, no.season, title, release_date)
  
  complete_ep_df <- rbind(complete_ep_df, df)
}

complete_ep_df %>%
  write_csv(file.path(bob_ross_path, "episode_data.csv"))
