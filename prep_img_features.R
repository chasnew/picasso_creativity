library(tidyverse)
library(imager)
# library(tensorflow)
# library(keras)
library(keras3)

# model loading
ml_loading <- function(feature_model) {
  if (feature_model == "vgg") {
    # vgg model
    base_model <- application_vgg16(weights = 'imagenet')
    base_model$trainable <- FALSE
    
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
    base_model$trainable <- FALSE
    
    # (None, 112, 112, 64)
    model.conv1 <- keras_model(inputs = base_model$input,
                               outputs = get_layer(base_model, 'conv1_conv')$output)
    # (None, 56, 56, 64)
    model.pool1 <- keras_model(inputs = base_model$input,
                               outputs = get_layer(base_model, 'pool1_pool')$output)
    # (None, 2048)
    model.fc <- keras_model(inputs = base_model$input,
                            outputs = get_layer(base_model, 'avg_pool')$output)
  } else if (feature_model == "effnet") {
    # EfficientNet model
    base_model <- application_efficientnet_b0(weights = 'imagenet')
    base_model$trainable <- FALSE
    
    # (None, 112, 112, 32)
    model.conv1 <- keras_model(inputs = base_model$input,
                               outputs = get_layer(base_model, 'stem_conv')$output)
    # (None, 112, 112, 32)
    model.pool1 <- keras_model(inputs = base_model$input,
                               outputs = get_layer(base_model, 'block1a_activation')$output)
    # (None, 1280)
    model.fc <- keras_model(inputs = base_model$input,
                            outputs = get_layer(base_model, 'avg_pool')$output)
  }
  
  
  ml_layers <- list(model.conv1, model.pool1, model.fc, base_model)
  return(ml_layers)
}


# split samples into multiple batches/groups
path_batching <- function(img_paths, batch_size) {
  
  img_n <- length(img_paths)
  
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
  
  path_batches <- split(img_paths, f = group_labels)
  return(path_batches)
}


# extract features from each batch of images
extract_features <- function(path_batches, ml_layers) {
  model.conv1 <- ml_layers[[1]]
  model.pool1 <- ml_layers[[2]]
  model.fc <- ml_layers[[3]]
  base_model <- ml_layers[[4]]
  
  low_features <- c()
  high_features <- c()
  
  # batch counter
  b_count <- 1
  
  for (path_batch in path_batches) {
    print(paste("batch", b_count))
    flush.console()
    b_count <- b_count + 1
    
    # tmp <- application_preprocess_inputs(base_model, x)
    # dim1 <- dim(tmp)[1]
    # tmp <- array(tmp, dim = c(dim1, 224, 224, 3))
    # tmp[1,,,1]
    # x[1,,,3] - tmp[1,,,1]
    # x[1,1,,1]
    # tmp[1,1,,1]
    
    # load images
    x <- map(path_batch,
             ~image_load(.x, target_size = c(224,224), color_mode = "rgb") %>% 
               image_to_array()
               # divide_by(255)
    ) # result in list
    x <- array(unlist(x), dim = c(224, 224, 3, length(x))) %>% # convert into an array
      aperm(c(4,1,2,3))
    
    x <- application_preprocess_inputs(base_model, x)
    x <- array(unlist(x), dim = c(dim(x)[1], 224, 224, 3))
    
    # summary(base_model)
    # test_layer <- keras_model(inputs = base_model$input,
    #                           outputs = get_layer(base_model, 'conv5_block3_out')$output)
    # test.feature <- test_layer |>
    #   predict(x)
    # test.feature %>% dim()
    # test.feature
    # mean(test.feature[1,,,5], na.rm=T)
    # avg_tmp <- apply(test.feature, c(1,4), mean, na.rm=T)
    # avg_tmp[1,]
    
    # extract image embeddings
    feature.conv1 <- model.conv1 %>% 
      predict(x) %>%
      rowMeans(dims = 2)
    feature.pool1 <- model.pool1 %>% 
      predict(x) %>% 
      rowMeans(dims = 2)
    feature.fc <- model.fc %>% 
      predict(x)
    
    # feature.fc[1,]
    
    low_features <- rbind(low_features, cbind(feature.conv1, feature.pool1))
    high_features <- rbind(high_features, feature.fc)
  }
  
  colnames(low_features) <- paste0("low_f", 1:dim(low_features)[2])
  colnames(high_features) <- paste0("high_f", 1:dim(high_features)[2])
  
  return(list(low_features, high_features))
}