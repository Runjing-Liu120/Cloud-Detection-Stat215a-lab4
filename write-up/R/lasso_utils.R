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


get_lasso_fit_2fold_cv <- function(image1, image2, smooth_features = FALSE, 
                             thresh = c(0.4, 0.6)){
  # return logistic-lasso model with lambda chosen by 2-fold CV
  # where the two folds are the two images
  # thresh are the probabilties below/above which we label 1 or -1
  # in between we label 0
  # if smooth_features = TRUE, we use the 8 smoothed features; if FALSE, we use 
  # the 8 original features; if NULL, we use all the features
  
  # whether to take smoothed features or not
  if(is.null(smooth_features)){
    feature_names <- colnames(image1)[-c(1,2,3)] 
  }else{if(smooth_features){
          feature_names <- colnames(image1)[grep("_smoothed", colnames(image1))]
        }else{
          feature_names <- c('NDAI','SD','CORR','DF','CF','BF','AF','AN')
        }}

  
  image1_labeled_indx <- which(image1$label != 0)
  image2_labeled_indx <- which(image2$label != 0)
  
  image1_features <- image1[image1_labeled_indx, feature_names]
  image2_features <- image2[image2_labeled_indx, feature_names]
  
  image1_labels <- droplevels(image1$label[image1_labeled_indx])
  image2_labels <- droplevels(image2$label[image2_labeled_indx])
  
  # fit to image 1
  print('fitting to image 1 ...')
  fit1 <- glmnet(as.matrix(image1_features), image1_labels, family = "binomial")
  
  # predict on image 2
  image2_prediction_prob <- 
    predict(fit1, newx = as.matrix(image2_features), s = fit1$lambda, 
            type = 'response')
  image2_prediction <- 1.0 * (image2_prediction_prob > thresh[2]) + 
    -1.0 * (image2_prediction_prob < thresh[1]) 
  
  # get predictive accuracy
  acc1 <- colMeans(image2_prediction == image2_labels)
  
  # fit to image 2
  print('fitting to image 2 ...')
  fit2 <- glmnet(as.matrix(image2_features), image2_labels, family = "binomial", 
                 lambda = fit1$lambda)
  
  # predict on image 1
  image1_prediction_prob <- 
    predict(fit2, newx = as.matrix(image1_features), s = fit2$lambda, 
            type = 'response')
  image1_prediction <- 1.0 * (image1_prediction_prob > thresh[2]) + 
    -1.0 * (image1_prediction_prob < thresh[1]) 
  
  # get predictive accuracy
  acc2 <- colMeans(image1_prediction == image1_labels)
  
  acc_avg <- 0.5 * (acc1 + acc2)
  lambda_best <- fit2$lambda[which.max(acc_avg)]
  
  # get fit on full data
  print('getting final fit ...')
  X <- as.matrix(rbind(image1_features, image2_features))
  y <- c(image1_labels, image2_labels)
  
  fit_final <- glmnet(X, y, family = "binomial", lambda = lambda_best)
  
  cv_results <- list(accuracy = acc_avg, lambda = fit1$lambda, 
                     lambda_best = lambda_best)
  print('done')
  return(list(fit = fit_final, 
              cv_results = cv_results))
}


get_lasso_prediction <- function(fit, image3){
  # takes in a lasso fit, from the 'get_lasso_fit_2fold_cv' function above
  # and gets predictions on the test image (image3 here)
  # returns predictive probabilities, predicted classes, and accuracy
  
  # check if the fit was on smoothed or nonsmoothed features
  smooth_features <- 'NDAI_smoothed' %in% rownames(fit$beta)
  # else the feature would be called 'NDAI' without the '_smoothed'
  
  # whether to take smoothed features or not
  if(smooth_features){
    feature_names <- colnames(image3)[grep("_smoothed", colnames(image3))]
  }else{
    feature_names <- c('NDAI','SD','CORR','DF','CF','BF','AF','AN')
  }
  
  labeled_indx <- which(image3$label != 0)
  labeled_image <- image3[labeled_indx, ]
  
  y <- droplevels(labeled_image$label)
  
  X <- labeled_image[, feature_names]
  
  y_pred_prob <- predict(fit, 
                         newx = as.matrix(X),
                         type = 'response') # returns a probability of being +1 (vs. -1)
  
  # get the class label from probability
  y_pred_class <- 1.0 * (y_pred_prob > 0.5) + (-1.0) * (y_pred_prob < 0.5)
  
  # get predictive accuracy
  pred_accuracy <- mean(y_pred_class == y)
  
  return(list(pred_prob = y_pred_prob, 
              pred_class = y_pred_class, 
              accuracy = pred_accuracy))
}

