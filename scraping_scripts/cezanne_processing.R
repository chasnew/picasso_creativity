library(tidyverse)
library(stringr)
library(rvest)
library(xml2)

home_dir <- path.expand("~")
picasso_path <- file.path(home_dir, "Library/CloudStorage/Box-Box/QuantifyingPicasso")
cezanne_path <- file.path(picasso_path, "cezanne")

URL <- "https://en.wikipedia.org/wiki/List_of_paintings_by_Paul_C%C3%A9zanne#/media/"
pg <- read_html(URL)
# extract painting tables from the page and turn them into a dataframe
tbls <- html_nodes(pg, css="table.sortable") %>% 
  html_table()
paint_tbls <- rbind(tbls[[1]], tbls[[2]], tbls[[3]], tbls[[4]])
paint_tbls <- paint_tbls %>% select(-Image)
colnames(paint_tbls) <- c("title", "year", "dimensions", "location", "cat_id")

# sum(str_detect(paint_tbls$cat_id, "R"))

# extract image src from each table's row
painting_links <- pg %>% 
  html_nodes(xpath = "//tr/td/figure/a/img") %>% 
  html_attr("src")
painting_links <- paste0("https:", painting_links)

# typos in cat_id from the Wikipedia page
paint_tbls$cat_id[93] <- "V 79R 102FWN 408"
paint_tbls$cat_id[96] <- "V 72R 108FWN 411"
paint_tbls$cat_id[410] <- "V 355R 354 FWN 772"
paint_tbls$cat_id[436] <- "V 282R 463FWN 458"

paint_tbls$cat_id

# separate out FWN id
cat_id_inds <- unlist(gregexpr('FWN', paint_tbls$cat_id))
cat_id_lens <- nchar(paint_tbls$cat_id)
paint_tbls$fwn_id <- str_replace(substr(paint_tbls$cat_id, cat_id_inds, cat_id_lens), " ", "_")

# separate out R id
r_ids <- str_replace(substr(paint_tbls$cat_id, 1, cat_id_inds-1), "^[^R]*", "")
r_ids <- str_replace(r_ids, " ", "_")
r_ids[str_detect(r_ids, "R", negate=TRUE)] <- NA # 6 rows don't have R id
paint_tbls$r_id <- r_ids

# pull images using the links
paint_tbls$img_link <- painting_links

# modifying links to get the bigger images
paint_tbls$img_link <- sub(".jpg.*jpg", ".jpg", paint_tbls$img_link)
paint_tbls$img_link <- sub(".tif.*jpg", ".tif", paint_tbls$img_link)
paint_tbls$img_link <- sub(".jpeg.*jpeg", ".jpeg", paint_tbls$img_link)
paint_tbls$img_link <- sub(".JPG.*JPG", ".JPG", paint_tbls$img_link)
paint_tbls$img_link <- sub(".png.*png", ".png", paint_tbls$img_link)
paint_tbls$img_link <- sub("/thumb", "", paint_tbls$img_link)

link_lengths <- nchar(paint_tbls$img_link)
paint_tbls$filetype <- sub(".", "", substr(paint_tbls$img_link, link_lengths-3, link_lengths))

# extract the first and last year specified
paint_tbls <- paint_tbls %>% 
  mutate(stripped_year = gsub("([^0-9])", "", year),
         startYear = as.integer(substr(stripped_year, 1, 4)),
         endYear = case_when(nchar(stripped_year) == 8 ~ as.integer(substr(stripped_year, 5, 8)),
                             nchar(stripped_year) == 6 ~ as.integer(paste0(substr(stripped_year, 1, 2),
                                                                           substr(stripped_year, 5, 6))),
                             .default = as.integer(stripped_year))) %>% 
  select(-stripped_year)

# iterate over each row of painting dataframe to download images
for(i in 1:nrow(paint_tbls)) {
  print(i)
  img_url <- paint_tbls$img_link[i]
  
  if (paint_tbls$filetype[i] == "png") {
    paint_file <- file.path(cezanne_path, "paintings", paste0(paint_tbls$fwn_id[i], ".png"))
  }
  else {
    paint_file <- file.path(cezanne_path, "paintings", paste0(paint_tbls$fwn_id[i], ".jpg"))
  }
  download.file(img_url, paint_file)
}

# png_inds <-(1:nrow(paint_tbls))[paint_tbls$filetype == "png"]
# paint_tbls[png_inds,]
# paint_tbls$fwn_id[298]

paint_tbls %>%
  write_csv(file.path(cezanne_path, "cezanne_data", "cezanne_paintings.csv"))
