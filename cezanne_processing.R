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

cat_id_inds <- unlist(gregexpr('FWN', paint_tbls$cat_id))
cat_id_lens <- nchar(paint_tbls$cat_id)
paint_tbls$fwn_id <- str_replace(substr(paint_tbls$cat_id, cat_id_inds, cat_id_lens), " ", "_")

# remove "/wiki/" from href and concatenate with wiki URL
# paint_tbls$img_link <- file.path(URL, str_replace(painting_links, "/wiki/", ""))

# next step
# remove "thumb/" and the duplicate .jpg file name at the end of the links
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

# extract only the first year specified
paint_tbls$startYear <- as.integer(substr(gsub("([^0-9])", "", paint_tbls$year), 1, 4))

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
  write_csv(file.path(cezanne_path, "cezanne_paintings.csv"))
