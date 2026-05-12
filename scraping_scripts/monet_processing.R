library(tidyverse)
library(stringr)
library(rvest)
library(xml2)

home_dir <- path.expand("~")
picasso_path <- file.path(home_dir, "Library/CloudStorage/Box-Box/QuantifyingPicasso")
monet_path <- file.path(picasso_path, "monet")

URL <- "https://en.wikipedia.org/wiki/List_of_paintings_by_Claude_Monet"
pg <- read_html(URL)
# extract painting tables from the page and turn them into a dataframe
tbls <- html_nodes(pg, css="table.sortable") %>% 
  html_table()
paint_tbls <- NULL

# For Water-Lily paintings W.1501 to W.1520 (table 8)
# For Water-Lily paintings W.1656 to W.1691 (table 10)
# For Water-Lily paintings W.1781 to W.1817 (table 12)
# see https://en.wikipedia.org/wiki/Water_Lilies_(Monet_series).

water_lily_title <- "For Water-Lily paintings"

for (i in 1:12) {
  tmp.tbl <- tbls[[i]][,1:5]
  colnames(tmp.tbl) <- c("title", "year", "location", "dimensions", "cat_id")
  
  tmp.tbl <- tmp.tbl %>% 
    filter(!grepl(water_lily_title, title, fixed = TRUE))
  
  if (i == 1) {
    paint_tbls <- tmp.tbl
  } else {
    paint_tbls <- rbind(paint_tbls, tmp.tbl)
  }
}

# extract image src from each table's row
painting_links <- pg %>% 
  html_nodes(xpath = "//tr/td/figure/a/img") %>% 
  html_attr("src")
painting_links <- paste0("https:", painting_links)
length(painting_links)

# rows with no images
# (1:nrow(paint_tbls))[paint_tbls$cat_id == "W.1844"]
no_img_inds <- c(2, 3, 4, 647, 1742)
paint_tbls <- paint_tbls[-no_img_inds,]

# dropping "Authenticity in doubt" row
dbt_authen_ind <- (1:nrow(paint_tbls))[str_detect(paint_tbls$cat_id, "Authenticity")]
paint_tbls <- paint_tbls[-dbt_authen_ind,]
painting_links <- painting_links[-dbt_authen_ind]

# remove ". " and "." from catalogue id end contatenate "W" with the number
paint_tbls$cat_id <- gsub("\\.", "", gsub("\\. ", ".", paint_tbls$cat_id))
# concatenate suffixes with "_"
paint_tbls$cat_id <- gsub(" ", "_", paint_tbls$cat_id)
# replace "/" with "-" in suffixes
paint_tbls$cat_id <- gsub("/", "-", paint_tbls$cat_id)

# extract the first and last year specified
paint_tbls$year[paint_tbls$cat_id == "W575"] <- "1880"

paint_tbls <- paint_tbls %>% 
  mutate(stripped_year = gsub("([^0-9])", "", year),
         startYear = as.integer(substr(stripped_year, 1, 4)),
         endYear = case_when(nchar(stripped_year) == 8 ~ as.integer(substr(stripped_year, 5, 8)),
                             nchar(stripped_year) == 6 ~ as.integer(paste0(substr(stripped_year, 1, 2),
                                                                           substr(stripped_year, 5, 6))),
                             .default = as.integer(stripped_year))) %>% 
  select(-stripped_year)

# pull images using the links
paint_tbls$img_link <- painting_links

# modifying links to get the bigger images
paint_tbls$img_link <- sub(".jpg.*jpg", ".jpg", paint_tbls$img_link)
paint_tbls$img_link <- sub(".tiff.*jpg", ".tiff", paint_tbls$img_link)
paint_tbls$img_link <- sub(".jpeg.*jpeg", ".jpeg", paint_tbls$img_link)
paint_tbls$img_link <- sub(".JPG.*JPG", ".JPG", paint_tbls$img_link)
paint_tbls$img_link <- sub(".png.*png", ".png", paint_tbls$img_link)
paint_tbls$img_link <- sub(".PNG.*PNG", ".PNG", paint_tbls$img_link)
paint_tbls$img_link <- sub("/thumb", "", paint_tbls$img_link)


link_lengths <- nchar(paint_tbls$img_link)
paint_tbls$filetype <- sub(".", "", substr(paint_tbls$img_link, link_lengths-3, link_lengths))

# iterate over each row of painting dataframe to download images
for(i in 203:nrow(paint_tbls)) {
  print(i)
  img_url <- paint_tbls$img_link[i]
  
  if (!is.na(img_url)) {
    paint_file <- file.path(monet_path, "paintings", paste0(paint_tbls$cat_id[i], ".", paint_tbls$filetype[i]))
    download.file(img_url, paint_file)
  }
}

paint_tbls %>% 
  filter(filetype == "gif")

paint_tbls[202,]
# png_inds <-(1:nrow(paint_tbls))[paint_tbls$filetype == "png"]
# paint_tbls[png_inds,]
# paint_tbls$fwn_id[298]

paint_tbls %>%
  write_csv(file.path(monet_path, "monet_data", "monet_paintings.csv"))
