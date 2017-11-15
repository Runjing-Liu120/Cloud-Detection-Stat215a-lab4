library(knitr)
library(tidyverse)
library(glmnet)
library(ranger)
library(dplyr)

source('../LASSO/smoothing_fun.R')
source('../LASSO/lasso_lib.R')

get_rf_prediction <- function(model, image){
  image_prediction_prob_rf <- predict(model, 
                                      dat = image, 
                                      type = 'response')
  
  image_prediction_rf <- get_labels_from_prob(image_prediction_prob_rf$predictions[, 2])
  accuracy <- mean(image$label == image_prediction_rf)
  return(accuracy)
}

get_rf_fit <- function(image, smooth_features = FALSE){
  labeled_indx <- which(image$label != 0)
  labeled_image <- image[labeled_indx, ]
  y <- droplevels(labeled_image$label)
  
  if(smooth_features){
    X <- image[, grep("_smoothed", colnames(image))] # get smoothed features
  }
  else{
    feature_names <- c('NDAI','SD','CORR','DF','CF','BF','AF','AN')
    X <- image[, feature_names]
  }
  data <- cbind(y, X)
  
  rf_fit <- ranger(y ~ ., data = data, 
                   num.trees = 500, 
                   probability = TRUE, 
                   importance = 'impurity')
  return(rf_fit)
}

add_neighbor_info <- function(image){
  image_l <- image %>%
    mutate(x = x - 1) %>% 
    select(-label)
  image_r <- image %>%
    mutate(x = x + 1) %>% 
    select(-label)
  image_u <- image %>%
    mutate(y = y + 1) %>% 
    select(-label)
  image_d <- image %>%
    mutate(y = y - 1) %>% 
    select(-label)
  image_neighbor <- merge(image, image_l, 
                          by = c('x', 'y'), suffixes = c('','_l'))
  image_neighbor <- merge(image_neighbor, image_r, 
                          by = c('x', 'y'), suffixes = c('','_r'))
  image_neighbor <- merge(image_neighbor, image_u, 
                          by = c('x', 'y'), suffixes = c('','_u'))
  image_neighbor <- merge(image_neighbor, image_d, 
                          by = c('x', 'y'), suffixes = c('','_d'))
  return(image_neighbor)
}



# Fit using random forest
# original model: fit a random forest to image 1, and examine the predictive accuracy on the second image. 



# neighboring model: Try to add neighboring information and run random forest again
image1_neighbor <- add_neighbor_info(image1)
image2_neighbor <- add_neighbor_info(image2)
image3_neighbor <- add_neighbor_info(image3)

# feature importancesby decrease in GINI coefficient:
sort(importance(rf_fit_orig) / sum(importance(rf_fit_orig)), decreasing = T)