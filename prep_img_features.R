library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)
library(imager)
library(tensorflow)
library(keras)

home_dir <- path.expand("~")
picasso_path <- file.path(home_dir, "Library/CloudStorage/Box-Box/QuantifyingPicasso")
target_cat <- "painting"
feature_model <- "resnet"

# artwork tabular data
art_data <- read.delim("raw_data/artwork1.csv", colClasses = "character") # /t as separator

# row whose columns are shifted
tmp_ind <- art_data[art_data$opp == "OPP.36:312",] %>% rownames()
art_data[tmp_ind, 2] <- paste(art_data[tmp_ind, 2], art_data[tmp_ind, 3])
for (i in 3:ncol(art_data)-1) {
  art_data[tmp_ind, i] <- art_data[tmp_ind, i+1]
}

# parse out date, month, and year variables from existing columns
art_data <- art_data %>% 
  separate(dateEnd, c("yearEnd","monthEnd","dayEnd"), remove = FALSE) %>% 
  separate(dateStart, c("yearStart","monthStart","dayStart"), remove = FALSE) %>% 
  mutate(yearEnd = as.numeric(yearEnd),
         monthEnd = as.numeric(monthEnd),
         dayEnd = as.numeric(dayEnd),
         yearStart = as.numeric(yearStart),
         monthStart = as.numeric(monthStart),
         dayStart = as.numeric(dayStart))

# remove weird data (7 entries)
art_data <- art_data %>%
  filter(category != "") %>% # empty rows
  filter(yearEnd != 0) # 2 entries w/ missing end dates

# select necessary columns
reduced_art <- art_data %>% 
  filter(category == target_cat) %>% 
  select(opp, title, category, yearStart)

# model loading

if (feature_model == "vgg") {
  # vgg model
  base_model <- application_vgg16(weights = 'imagenet')
  # intermediate layers
  model.conv1 <- keras_model(inputs = base_model$input,
                             outputs = get_layer(base_model, 'block1_conv1')$output)
  model.pool1 <- keras_model(inputs = base_model$input,
                             outputs = get_layer(base_model, 'block1_pool')$output)
  model.fc <- keras_model(inputs = base_model$input,
                           outputs = get_layer(base_model, 'fc2')$output)
} else if (feature_model == "resnet") {
  # resnet model
  base_model <- application_resnet50(weights = 'imagenet')
  # (None, 112, 112, 64)
  model.conv1 <- keras_model(inputs = base_model$input,
                             outputs = get_layer(base_model, 'conv1_conv')$output)
  # (None, 56, 56, 64)
  model.pool1 <- keras_model(inputs = base_model$input,
                             outputs = get_layer(base_model, 'pool1_pool')$output)
  # (None, 2048)
  model.fc <- keras_model(inputs = base_model$input,
                               outputs = get_layer(base_model, 'avg_pool')$output)
}

print("Completed loading models")


# within-year batch size
batch_size <- 75

# input year range (min = 1889, max = 1973)
start_year <- 1941
end_year <- 1973
arg_year_range <- start_year:end_year

# retrieve file names (may need to edit to subset years)
year_list <- reduced_art %>% 
  pull(yearStart) %>% unique()

truc_yearlist <- arg_year_range[arg_year_range %in% year_list]

# reconstruct filename from OPP
reconstruct_fn <- function(opp_str) {
  return(paste0("yopp", substr(opp_str, 5, 6), "-", substr(opp_str, 8, 10), ".jpg"))
}

img_dir <- file.path(picasso_path, "data_from_OPP", "OPP_images")
feature_dir <- file.path(picasso_path, "data_from_OPP", "image_features")

# track processed year and opp ids
trackl_name <- paste0("track_list_", target_cat, "_", feature_model, ".Rdata")
if (file.exists(trackl_name)) {
  load(trackl_name)
} else {
  track_list <- list(year = c(), opp = c(), not_found = c())
}

for (year in truc_yearlist) {
  # enable local access to Box folder
  tmp_filelist <- list.files(file.path(img_dir, year, "ythumbs"))
  
  print(paste("year:", year))
  flush.console()
  
  # retrieve filepaths of the year
  sample_opps <- reduced_art %>% 
    filter(yearStart == year) %>% 
    pull(opp)
  
  opp_paths <- sample_opps %>% 
    reconstruct_fn() %>% 
    file.path(img_dir, year, "ythumbs", .)
  
  # check if all files exist
  not_found <- opp_paths[!file.exists(opp_paths)]
  if (length(not_found) > 0) {
    opp_paths <- opp_paths[file.exists(opp_paths)]
  }

  # split samples into multiple batches/groups
  img_n <- length(opp_paths)
  
  # check if numger of images is smaller than batch size
  if (img_n < batch_size) {
    n_batch <- 1
    group_labels <- rep(1, img_n)
  } else {
    n_batch <- floor(img_n/batch_size)
    group_labels <- rep(1:n_batch, each = batch_size)
    rem <- img_n %% batch_size
    
    if (rem != 0) {
      group_labels <- c(group_labels, rep(n_batch+1, rem))
      n_batch = n_batch + 1
    }
  }
  
  print(paste("image number:", img_n))
  print(paste("batch number:", n_batch))
  
  path_batches <- split(opp_paths, f = group_labels)
  
  # extract features from each batch of images
  low_features <- c()
  high_features <- c()
  
  # batch counter
  b_count <- 1
  
  for (path_batch in path_batches) {
    print(paste("batch", b_count))
    flush.console
    b_count <- b_count + 1
    
    # load images
    x <- map(path_batch,
             ~image_load(.x, target_size = c(224,224), grayscale = FALSE) %>% 
               image_to_array() %>% 
               divide_by(255)
    ) # result in list
    x <- array(unlist(x), dim = c(224, 224, 3, length(x))) %>% # convert into an array
      aperm(c(4,1,2,3))
    
    # extract image embeddings
    feature.conv1 <- model.conv1 %>%
      predict(x) %>%
      rowMeans(dims = 2)
    feature.pool1 <- model.pool1 %>% 
      predict(x) %>% 
      rowMeans(dims = 2)
    feature.fc <- model.fc %>% 
      predict(x)
    
    low_features <- rbind(low_features, cbind(feature.conv1, feature.pool1))
    high_features <- rbind(high_features, feature.fc)
  }
  
  # store features in Box folder as csv
  colnames(low_features) <- paste0("low_f", 1:dim(low_features)[2])
  colnames(high_features) <- paste0("high_f", 1:dim(high_features)[2])
  
  low_features %>% 
    data.table() %>% 
    fwrite(file.path(feature_dir, paste0("low_feature_", target_cat, "_", feature_model, ".csv")), append = TRUE)
  
  high_features %>% 
    data.table() %>% 
    fwrite(file.path(feature_dir, paste0("high_feature_", target_cat, "_", feature_model, ".csv")), append = TRUE)
  
  # store current year
  track_list[["year"]] <- c(track_list[["year"]], year)
  
  # store opp ids
  track_list[["opp"]] <- c(track_list[["opp"]], sample_opps)
  
  # store not found files
  track_list[["not_found"]] <- c(track_list[["not_found"]], not_found)
  
  # store processed year and opp ids
  save(track_list, file = trackl_name)
}