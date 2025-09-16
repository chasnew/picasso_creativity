library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)
library(imager)
library(tensorflow)
library(keras)

home_dir <- path.expand("~")
picasso_path <- file.path(home_dir, "Library/CloudStorage/Box-Box/QuantifyingPicasso")
bob_ross_path <- file.path(picasso_path, "bob_ross")
br_data_path <- file.path(bob_ross_path, "bob_ross_data")
feature_model <- "resnet"


# artwork tabular data
art_data <- read.csv(file.path(br_data_path, "bob_ross_paintings.csv")) %>% 
  select(painting_index, painting_title, season, episode, num_colors)

# The Joy of Painting episode data with *date*
ep_data <- read.csv(file.path(br_data_path, "episode_data.csv"))
names(ep_data)[2] <- "episode"


art_data <- merge(x = art_data, y = ep_data, by = c("season", "episode")) %>% 
  select(painting_index, season, episode, title, release_date)

img_dir <- file.path(bob_ross_path, "paintings")



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

# enable local access to Box folder
br_img_paths <- list.files(file.path(img_dir))
br_img_paths <- file.path(img_dir, br_img_paths)
  
# split samples into multiple batches/groups
img_n <- length(br_img_paths)
  
# check if number of images is smaller than batch size
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

path_batches <- split(br_img_paths, f = group_labels)

# extract features from each batch of images
low_features <- c()
high_features <- c()

# batch counter
b_count <- 1

for (path_batch in path_batches) {
  print(paste("batch", b_count))
  flush.console()
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
  fwrite(file.path(br_data_path, paste0("low_feature_", feature_model, ".csv")), append = TRUE)

high_features %>% 
  data.table() %>% 
  fwrite(file.path(br_data_path, paste0("high_feature_", feature_model, ".csv")), append = TRUE)
