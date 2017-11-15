get_logistic_fit <- function(image, smooth_features = FALSE){
  # takes an image that is the output of prep_image
  # and returns a logistic regression fit
  
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
  
  fit <- glm(y ~ ., data = data, family = 'binomial')
  
  return(fit)
}

get_logistic_prediction <- function(fit, image_test){
  # takes in a logistic fit, a new image and predicts on that image
  # returns predicted probabilities, the predicted class labels, and the
  # predictive accuracy
  
  # check if the fit was on smoothed or nonsmoothed features
  smooth_features <- 'NDAI_smoothed' %in% names(fit$coefficients)
  # else the feature would be called 'NDAI' without the '_smoothed'
  
  labeled_indx <- which(image_test$label != 0)
  labeled_image <- image_test[labeled_indx, ]
  
  y <- droplevels(labeled_image$label)
  
  if(smooth_features){
    X <- image_test[, grep("_smoothed", colnames(image_test))] # get smoothed features
  } else{
    feature_names <- c('NDAI','SD','CORR','DF','CF','BF','AF','AN')
    X <- image_test[, feature_names]
  }
  
  data <- cbind(y, X)
  y_pred_prob <- predict(fit, 
                newdata = X,
                type = 'response') # returns a probability of being +1 (vs. -1)
  
  # get the class label from probability
  y_pred_class <- 1.0 * (y_pred_prob > 0.5) + (-1.0) * (y_pred_prob < 0.5)
  
  # get predictive accuracy
  pred_accuracy <- mean(y_pred_class == y)
  
  return(list(pred_prob = y_pred_prob, 
              pred_class = y_pred_class, 
              accuracy = pred_accuracy))
}