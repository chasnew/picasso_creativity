library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)
library(tensorflow)
library(keras)

category <- "painting"

# artwork tabular data
art_data <- read.delim("raw_data/artwork1.csv", colClasses = "character") # /t as separator

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

# remove weird data (8 entries)
art_data <- art_data %>%
  filter(category != "25~26-March/1936") %>% # shifted column (fixable)
  filter(category != "") %>% # empty rows
  filter(yearEnd != 0) # 2 entries w/ missing end dates

# select necessary columns
reduced_art <- art_data %>% 
  filter(category == category) %>% 
  select(opp, title, category, yearStart)

# model loading
base_model <- application_vgg16(weights = 'imagenet')
# intermediate layers
model.conv1 <- keras_model(inputs = base_model$input,
                           outputs = get_layer(base_model, 'block1_conv1')$output)
model.pool1 <- keras_model(inputs = base_model$input,
                           outputs = get_layer(base_model, 'block1_pool')$output)
model.fc2 <- keras_model(inputs = base_model$input,
                         outputs = get_layer(base_model, 'fc2')$output)

batch_size <- 30

# retrieve file names
year_list <- reduced_art %>% 
  pull(yearStart) %>% unique()

sample_paintings <- paintings %>% 
  filter(yearStart >= 1900, yearStart <= 1970) %>% 
  group_by(yearStart) %>% 
  do(head(., batch_size))

# reconstruct filename from OPP
reconstruct_fn <- function(opp_str) {
  return(paste0("yopp", substr(opp_str, 5, 6), "-", substr(opp_str, 8, 10), ".jpg"))
}

img_dir <- "~/Box/QuantifyingPicasso/data_from_OPP/OPP_images"
feature_dir <- "~/Box/QuantifyingPicasso/data_from_OPP/image_features"

# enable local access to Box folders
for (year in year_list) {
  tmp_filelist <- list.files(file.path(img_dir, year, "ythumbs"))
}

# split samples into multiple batches/groups
img_n <- length(sample_images)
n_batch <- floor(img_n/batch_size)
group_labels <- rep(1:n_batch, each = batch_size)
rem <- img_n %% batch_size

if (rem != 0) {
  group_labels <- c(group_labels, rep(n_batch+1, rem))
  n_batch = n_batch + 1
}

path_batches <- split(full_paths, f = group_labels)

# extract features from each batch of images
low_features <- c()
high_features <- c()

for (path_batch in path_batches) {
  # constrctu file paths
  sample_opps <- sample_paintings %>% 
    filter(yearStart == year) %>% 
    pull(opp)
  
  opp_paths <- sample_opps %>% 
    reconstruct_fn() %>% 
    file.path(img_dir, year, "ythumbs", .)
  
  x <- map(opp_paths,
           ~image_load(.x, target_size = c(224,224), grayscale = FALSE) %>% 
             image_to_array() %>% 
             divide_by(255)
  ) # result in list
  x <- array(unlist(x), dim = c(224, 224, 3, length(x))) %>% # convert into an array
    aperm(c(4,1,2,3))
  
  feature.conv1 <- model.conv1 %>%
    predict(x) %>%
    rowMeans(dims = 2)
  feature.pool1 <- model.pool1 %>% 
    predict(x) %>% 
    rowMeans(dims = 2)
  feature.fc2 <- model.fc2 %>% 
    predict(x)
  
  low_features <- rbind(low_features, cbind(feature.conv1, feature.pool1))
  high_features <- rbind(high_features, feature.fc2)
}

# store features in Box folder as csv
colnames(low_features) <- paste0("low_f", 1:336)
colnames(high_features) <- paste0("high_f", 1:4096)

low_features <- low_features %>% 
  data.table()

high_features <- high_features %>% 
  data.table()