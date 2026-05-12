library(tidyverse)
library(truncnorm)
library(rsample)

# (n_iter/num_cycles)/1.25: tuning relative length between exploit vs explore
ars_step_fun <- function(dimn = 1, t = 1, n_iter = 1000, num_cycles = 3) {
  search_param <- case_when(dimn < 0 ~ 1,
                            (t %% (n_iter/num_cycles)) < 
                              ((n_iter/num_cycles)/1.25) ~ 0,
                            TRUE ~ 1)
  step_size <- rtruncnorm(1, a=0, b=1, mean=search_param, sd=0.05)
  direction <- ((-1)^rbinom(n=length(search_param), size=1, prob=0.5))
  return(step_size * direction)
}

constrict_step_fun <- function(dimn = 1, t = 1, dim_scale = 1, k = 1/10) {
  search_param <- case_when(dimn < 0 ~ 1,
                            TRUE ~ 1 - (1/(1 + exp(-k*(t - (dim_scale*dimn))))))
  step_size <- rtruncnorm(1, a=0, b=1, mean=search_param, sd=0.05)
  direction <- ((-1)^rbinom(n=length(search_param), size=1, prob=0.5))
  return(step_size * direction)
}

expand_step_fun <- function(dimn = 1, t = 1, dim_scale = 1, k = 1/10) {
  search_param <- case_when(dimn < 0 ~ 1,
                            TRUE ~ 1/(1 + exp(-k*(t - (dim_scale*dimn)))))
  step_size <- rtruncnorm(1, a=0, b=1, mean=search_param, sd=0.05)
  direction <- ((-1)^rbinom(n=length(search_param), size=1, prob=0.5))
  return(step_size * direction)
}

simulate_mvt <- function(fixed_dims, novel_dims, exist_dates,
                         mvt_model = "expansive", num_cycles = 40) {
  
  total_dims <- fixed_dims + novel_dims # total number of dimensionality in the model
  n_iter <- length(exist_dates) # number of steps
  
  # takes 100 steps for the curve to approach 1
  dim_scale <- (n_iter / novel_dims) - (1800 / novel_dims) - 1
  
  # initialize the first state in style space
  state_i <- runif(total_dims, -1, 1)
  states <- data.frame(matrix(0, ncol = total_dims, nrow = 1))
  states[1,] <- state_i
  x <- paste0("dim", 1:total_dims)
  colnames(states) <- x
  t <- 2
  
  for (i in 2:length(exist_dates)) {
    if (mvt_model == "expansive") {
      change_i <- expand_step_fun(c(rep(-1, fixed_dims), 1:novel_dims), t = t, dim_scale = dim_scale)
    } else if (mvt_model == "constrictive") {
      change_i <- constrict_step_fun(c(rep(-1, fixed_dims), 1:novel_dims), t = t, dim_scale = dim_scale)
    } else if (mvt_model == "ars") {
      change_i <- ars_step_fun(c(rep(-1, fixed_dims), rep(1, novel_dims)), t = t, n_iter = n_iter,
                               num_cycles = num_cycles)
    }
    
    
    state_iminus1 <- state_i
    state_i <- state_iminus1 + change_i
    
    # clip the value of state_i by a set threshold (-1, 1)
    state_i <- pmax(pmin(state_i, 1), -1)
    
    # then store state vectors
    states <- rbind(states, state_i)
    t <- t + 1
  }
  
  states$dateStart <- exist_dates
  states <- states %>%
    select(dateStart, dim1:dim30)
  
  return(states)
}