library(tidyverse)
library(data.table)
library(lubridate)
library(rsample)

stderror <- function(x) sd(x)/sqrt(length(x))

# calculate ordinal step sizes (based on cosine similarity) and turn angles (cosine similarity)
ordinal_movement <- function(ordered_pca) {
  
  trimmed_pca1 <- ordered_pca[-1,] # remove the first row
  trimmed_pca2 <- ordered_pca[-nrow(ordered_pca),] # remove the last row
  
  ## Cosine step sizes
  abs_pca <- ordered_pca %>%
    mutate(across(contains("_pc"), ~ . ^2)) %>%
    select(contains("_pc")) %>%
    rowSums() %>%
    as.vector() %>%
    sqrt()
  
  dot_pca <- rowSums(trimmed_pca1*trimmed_pca2)
  size_product <- abs_pca[-1]*abs_pca[-length(abs_pca)]
  
  step_sizes <- (1 - dot_pca/size_product)/2
  
  ## Directional changes (cosine similarity)
  mvt_df <- trimmed_pca1 - trimmed_pca2
  
  trimmed_mvt1 <- mvt_df[-1,] # remove the first row
  trimmed_mvt2 <- mvt_df[-nrow(mvt_df),] # remove the last row
  
  ### Euclidean distance of the step
  abs_step_sizes <- mvt_df %>%
    mutate(across(contains("_pc"), ~ . ^2)) %>%
    select(contains("_pc")) %>%
    rowSums() %>%
    as.vector() %>%
    sqrt()
  
  dot_pca <- rowSums(trimmed_mvt1*trimmed_mvt2)
  size_product <- abs_step_sizes[-1]*abs_step_sizes[-length(abs_step_sizes)]
  
  cos_sims <- dot_pca/size_product
  ordinal_steps <- list(step_sizes, cos_sims)
  return(ordinal_steps)
}

# Burstiness functions
detect_burst <- function(ts, thresh=2, scale=TRUE) {
  if (scale) {
    scaled_ts <- scale(ts, center = TRUE, scale = TRUE)[,1]
    return(scaled_ts > thresh)
  } else {
    return(ts > thresh)
  }
}

extract_iei <- function(burst_vector) {
  interval_list <- c()
  interval <- 0
  burst_count <- 0
  for (i in 1:length(burst_vector)) {
    if (burst_vector[i]) {
      if (burst_count == 0) {
        interval_list <- c(interval_list, NA)
      } else {
        interval_list <- c(interval_list, interval + 1)
      }
      interval <- 0
      burst_count <- burst_count + 1
    } else {
      interval <- interval + 1
    }
  }
  return(interval_list)
}

burstiness <- function(iei) {
  sd_iei <- sd(iei)
  avg_iei <- mean(iei)
  
  return((sd_iei - avg_iei)/(sd_iei + avg_iei))
}

# sliding window analysis of "day-to-day" movements of the artist
sliding_window_mvt <- function(ordered_pca, first_date, last_date, window_size,
                               wstep_size, wstep_unit, sample.size = 50,
                               sample.method = "first") {
  
  # initializing first window
  wfirst_date <- first_date
  wlast_date <- first_date + window_size
  
  # list to store results
  result_list <- list(cstep_avg = c(),
                      cstep_sd = c(),
                      dim90 = c(),
                      pratio = c(),
                      loc_burst = c(),
                      true_n = c(),
                      sample_sd = c())
  
  # create window step size for lubridate
  if (wstep_unit == "days") {
    wstep <- days(wstep_size)
  } else if (wstep_unit == "weeks") {
    wstep <- weeks(wstep_size)
  } else if (wstep_unit == "months") {
    wstep <- months(wstep_size)
  } else {
    wstep <- years(wstep_size)
  }
  
  wstep_str <- paste(wstep_size, wstep_unit)
  
  # create vectors of start and end dates for each window
  start_date_seq <- seq(first_date, last_date + wstep - window_size + days(1), by = wstep_str)
  end_date_seq <- seq(wlast_date - days(1), last_date + wstep, by = wstep_str)
  
  while (wlast_date <= (last_date + wstep)) {
    print(paste("start date =", wfirst_date))
    
    # filtering data using the window size
    window_pca <- ordered_pca %>% 
      filter(target_date >= wfirst_date, target_date < wlast_date)
    
    print(paste("number of rows =", nrow(window_pca)))
    
    # check if there's only one data point
    if (nrow(window_pca) <= 1) {
      result_list$cstep_avg <- c(result_list$cstep_avg, NA)
      result_list$cstep_sd <- c(result_list$cstep_sd, NA)
      result_list$dim90 <- c(result_list$dim90, NA)
      result_list$pratio <- c(result_list$pratio, NA)
      result_list$loc_burst <- c(result_list$loc_burst, NA)
      result_list$true_n <- c(result_list$true_n, nrow(window_pca))
      result_list$sample_sd <- c(result_list$sample_sd, NA)
      # result_list$sample_n <- c(result_list$sample_n, nrow(window_pca))
      # result_list$date_burst <- c(result_list$date_burst, NA)
      
      wfirst_date <- wfirst_date + wstep
      wlast_date <- wlast_date + wstep
      next
    }
    
    trimmed_pca1 <- window_pca[-1,] # remove the first row
    trimmed_pca2 <- window_pca[-nrow(window_pca),] # remove the last row
    
    # movement vectors
    mvt_df <- trimmed_pca1 - trimmed_pca2
    
    ## Cosine step sizes
    abs_step_sizes <- window_pca %>%
      mutate(across(contains("_pc"), ~ . ^2)) %>%
      select(contains("_pc")) %>%
      rowSums() %>%
      as.vector() %>%
      sqrt()
    
    trimmed_pca1 <- trimmed_pca1 %>% select(-target_date)
    trimmed_pca2 <- trimmed_pca2 %>% select(-target_date)
    
    dot_pca <- rowSums(trimmed_pca1*trimmed_pca2)
    size_product <- abs_step_sizes[-1]*abs_step_sizes[-length(abs_step_sizes)]
    
    cstep_sizes <- (1 - dot_pca/size_product)/2
    
    result_list$cstep_avg <- c(result_list$cstep_avg, mean(cstep_sizes))
    result_list$cstep_sd <- c(result_list$cstep_sd, sd(cstep_sizes))
    
    
    # Stylistic Dimensionality
    features.subi <- window_pca %>% 
      select(-target_date)
    
    # sum(sapply(features.subi[1:30], sd))
    
    if (sample.method == "random") {
      features.subsample <- features.subi %>% slice_sample(n=sample.size)
    } else if(sample.method == "first") {
      features.subsample <- features.subi %>% slice_head(n=sample.size)
    } else {
      features.subsample <- features.subi
    }
    
    # print(paste("number of subsamples =", nrow(features.subsample)))
    # result_list$sample_n <- c(result_list$sample_n, nrow(features.subsample))
    
    result_list$sample_sd <- c(result_list$sample_sd, sum(sapply(features.subsample[1:30], sd)))
    
    features.subi.pca <- prcomp(features.subsample)
    cum.sdev.prop <- cumsum(features.subi.pca$sdev/sum(features.subi.pca$sdev))
    
    # sum(features.subi.pca$sdev)
    
    ## quantifying the number of dimensions to explain x proportion of sdev
    style_dim90 <- sum((cum.sdev.prop < .90)) + 1
    
    ## calculating participation rate
    pca.variance <- features.subi.pca$sdev^2 # sdev -> variance
    pca.var.norm <- pca.variance / sum(pca.variance) # normalization
    p.ratio <- 1/sum(pca.var.norm^2)
    
    if (nrow(features.subi) < 20) {
      style_dim90 <- NA
      p.ratio <- NA
    }
    
    result_list$dim90 <- c(result_list$dim90, style_dim90)
    result_list$pratio <- c(result_list$pratio, p.ratio)
    
    
    # only calculate if there are at least 2 movement vectors
    if (nrow(mvt_df) > 1) {
      # 0 bigsteps will be NA
      ## Burstiness
      loc_threshold <- mean(cstep_sizes) + (1*sd(cstep_sizes))
      
      loc_bigstep <- detect_burst(cstep_sizes, thresh=loc_threshold, scale=F)
      step_iei1 <- extract_iei(loc_bigstep)[-1]
      
      result_list$loc_burst <- c(result_list$loc_burst, burstiness(step_iei1))
      
      # date burstiness (days that Picasso started artworks)
      # avail_dates <- window_pca$dateStart
      # tmp_date_seq <- seq(wfirst_date, wlast_date - days(1), by = "1 day")
      # 
      # burst_inds <- match(avail_dates, tmp_date_seq)
      # date_burst <- rep(0, length(tmp_date_seq))
      # date_burst[burst_inds] <- 1
      # 
      # date_burst <- as.logical(date_burst)
      # date_iei <- extract_iei(date_burst)[-1]
      # 
      # result_list$date_burst <- c(result_list$date_burst, burstiness(date_iei))
      
    } else {
      result_list$loc_burst <- c(result_list$loc_burst, NA)
      # result_list$date_burst <- c(result_list$date_burst, NA)
    }
    
    result_list$true_n <- c(result_list$true_n, nrow(window_pca))
    
    wfirst_date <- wfirst_date + wstep
    wlast_date <- wlast_date + wstep
  }
  
  slidw_df <- data.frame(result_list)
  
  slidw_df$start_date <- start_date_seq
  slidw_df$end_date <- end_date_seq
  
  slidw_df <- slidw_df[,c(8,9,1:7)] # move start and end date to the first two columns
  return(slidw_df)
}



parametric_coarse_grain <- function(ordered_df, wsize_list, step_thresh, cos_thresh) {
  
  result_list <- list(window_size = c(),
                      step_avg = c(),
                      step_se = c(),
                      dir_avg = c(),
                      dir_se = c(),
                      step_burst = c(),
                      dir_burst = c(),
                      step_dir_cor = c(),
                      step_dir_lag = c(),
                      dir_step_lag = c(),
                      step_acf = c(),
                      dir_acf = c())
  
  for (window_size in wsize_list) {
    result_list$window_size <- c(result_list$window_size, window_size)
    
    first_date <- min(ordered_df$dateStart)
    ordered_df <- ordered_df %>% 
      mutate(day_count = as.numeric(dateStart - first_date),
             date_group = floor(day_count/window_size))
    
    agg_pca <- ordered_df %>% 
      group_by(date_group) %>% 
      summarize(across(contains("_pc"), mean))
    
    trimmed_pca1 <- agg_pca[-1,] # remove the first row
    trimmed_pca2 <- agg_pca[-nrow(agg_pca),] # remove the last row
    
    
    ## Cosine step sizes
    abs_step_sizes <- agg_pca %>%
      mutate(across(contains("_pc"), ~ . ^2)) %>%
      select(contains("_pc")) %>%
      rowSums() %>%
      as.vector() %>%
      sqrt()
    
    dot_pca <- rowSums(trimmed_pca1[,-1]*trimmed_pca2[,-1])
    size_product <- abs_step_sizes[-1]*abs_step_sizes[-length(abs_step_sizes)]
    
    cstep_sizes <- (1 - dot_pca/size_product)/2 # normalizing step to 0-1 (1 = max distance)
    
    result_list$step_avg <- c(result_list$step_avg, mean(cstep_sizes))
    result_list$step_se <- c(result_list$step_se, stderror(cstep_sizes))
    
    
    ## Directional changes (cosine similarity)
    mvt_df <- trimmed_pca1 - trimmed_pca2
    
    trimmed_mvt1 <- mvt_df[-1,-1] # remove the first row
    trimmed_mvt2 <- mvt_df[-nrow(mvt_df),-1] # remove the last row
    
    ### Euclidean distance of the step
    abs_step_sizes <- mvt_df %>% 
      mutate(across(contains("_pc"), ~ . ^2)) %>% 
      select(contains("_pc")) %>% 
      rowSums() %>% 
      as.vector() %>% 
      sqrt()
    
    dot_pca <- rowSums(trimmed_mvt1*trimmed_mvt2)
    size_product <- abs_step_sizes[-1]*abs_step_sizes[-length(abs_step_sizes)]
    
    cos_sims <- dot_pca/size_product
    
    result_list$dir_avg <- c(result_list$dir_avg, mean(cos_sims))
    result_list$dir_se <- c(result_list$dir_se, stderror(cos_sims))
    
    
    ## Burstiness
    burst_df <- data.frame(date_diff = mvt_df$date_group)
    burst_df$bigstep <- detect_burst(cstep_sizes, thresh=step_thresh)
    step_iei <- extract_iei(burst_df)
    
    result_list$step_burst <- c(result_list$step_burst, burstiness(step_iei))
    
    burst_df$fwd <- c(detect_burst(cos_sims, thresh=cos_thresh, scale=FALSE), FALSE)
    dir_iei <- extract_iei(burst_df, burst_ind = 3)
    
    result_list$dir_burst <- c(result_list$dir_burst, burstiness(dir_iei))
    
    ## Cross- & auto-correlations
    ccf_results <- ccf(cstep_sizes, cos_sims, lag.max = 1, plot = F)
    
    result_list$step_dir_cor <- c(result_list$step_dir_cor, ccf_results$acf[2])
    result_list$step_dir_lag <- c(result_list$step_dir_lag, ccf_results$acf[3])
    result_list$dir_step_lag <- c(result_list$dir_step_lag, ccf_results$acf[1])
    
    step_acf <- acf(cstep_sizes, lag.max = 1, plot = F)$acf[2]
    dir_acf <- acf(cos_sims, lag.max = 1, plot = F)$acf[2]
    
    result_list$step_acf <- c(result_list$step_acf, step_acf)
    result_list$dir_acf <- c(result_list$dir_acf, dir_acf)
  }
  
  agg_results <- data.frame(result_list)
  return(agg_results)
}


# generate a heatmap of step sizes and turn angles
# (ignore the initial step because it has no directional change)
mvt_heatmap <- function(step_sizes, cos_sims, artist) {
  mvt.df <- tibble(step_size = step_sizes[2:length(step_sizes)], cos_sim = cos_sims)
  mvt.probs1 <- ks::kde(mvt.df,
                        xmin=c(0, -1),
                        xmax=c(+1, +1))
  
  # probability values in each grid
  kde_matrix1 <- mvt.probs1$estimate
  
  # eval.points = representative value of each bin
  rownames(kde_matrix1) <- mvt.probs1$eval.points[[1]]
  colnames(kde_matrix1) <- mvt.probs1$eval.points[[2]]
  
  # reshape a matrix into a long format data
  kde_tb1 <- reshape2::melt(kde_matrix1) %>%
    rename(step_size = Var1, cos_sim = Var2)
  
  heatmap_plot <- kde_tb1 %>%
    ggplot(aes(x=step_size, y=cos_sim)) + 
    geom_tile(aes(fill=value)) +
    geom_contour(aes(z=value),
                 color="black",
                 breaks = c(0.4, 1, 1.8, 2.4)) +
    scale_x_continuous(name = expression("Stylistic Change, "*Delta*"("*s[t]*","*s[t+1]*")"), expand=c(0,0)) +
    scale_y_continuous(name = expression("Direction Change, "*tau*"("*v[t]*","*v[t+1]*")"), expand=c(0,0)) +
    scale_fill_gradient(name = "Density", low = "blue", high = "red") +
    ggtitle(artist) +
    theme_classic() +
    theme(legend.position = "none",
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title=element_text(size=14),
          panel.border = element_rect(colour = "black",
                                      fill=NA, linewidth=3))
  
  return(heatmap_plot)
}