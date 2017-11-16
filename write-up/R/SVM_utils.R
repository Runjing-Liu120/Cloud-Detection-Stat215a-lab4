library(e1071)
library(foreach)
library(doParallel)

get_svm_fit_2fold_cv <- function(image1, image2, smooth_features = FALSE, 
                                 sample = FALSE){
  # return svm model with cost chosen by 2-fold CV
  # where the two folds are the two images
  # if smooth_features = TRUE, we use the smoothed features; if FALSE, we use 
  # the original features;
  # for speed, svm will only take in NDAI, CORR, and DF as features
  # sample = TRUE only for debugging purposes ...
  
  # whether to take smoothed features or not
  if(smooth_features){
    feature_names <- c('NDAI_smoothed','CORR_smoothed','DF_smoothed')
  }else{
    feature_names <- c('NDAI','CORR','DF')
  }
  
  image1_labeled_indx <- which(image1$label != 0)
  image2_labeled_indx <- which(image2$label != 0)
  
  image1_features <- image1[image1_labeled_indx, feature_names]
  image2_features <- image2[image2_labeled_indx, feature_names]
  
  image1_labels <- droplevels(image1$label[image1_labeled_indx])
  image2_labels <- droplevels(image2$label[image2_labeled_indx])
  
  image1_df <- cbind(image1_labels, image1_features)
  image2_df <- cbind(image2_labels, image2_features)
  
  colnames(image1_df)[1] <- 'y'
  colnames(image2_df)[1] <- 'y'
  
  if(sample){
    smpl_indx1 <- sample(dim(image1_df)[1], 100)
    smpl_indx2 <- sample(dim(image2_df)[1], 100)
    image1_df <- image1_df[smpl_indx1, ]
    image2_df <- image2_df[smpl_indx2, ]
  }
  nCores <- 4
  registerDoParallel(nCores) 
  
  cost = c(0.1, 1.0, 10, 100)
  cv_results <- foreach(i = 1:length(cost)) %dopar% {
    # fit to image 1
    print('testing cost = ')

    print('fitting to image 1 ..., ')
    fit1 <- svm(y ~ ., data = image1_df, cost = cost[i])
    
    # predict on image 2
    image2_prediction <- 
      predict(fit1, newdata = image2_features)

    # get predictive accuracy
    acc1 <- mean(image2_prediction == image2_labels)
    
    # fit to image 2
    print('fitting to image 2 ...')
    fit2 <- svm(y ~ ., data = image2_df, cost = cost[i])
    
    # predict on image 1
    image1_prediction <- 
      predict(fit2, newdata = image1_features)
  
    # get predictive accuracy
    acc2 <- mean(image1_prediction == image1_labels)
  
    acc_avg <- 0.5 * (acc1 + acc2)
    cv_result <- c(acc_avg, cost[i])
  }
  
  # this is hacky but it should work
  cv_results_vec <- unlist(cv_results)
  accuracies <- cv_results_vec[seq(from = 1, to = length(cv_results_vec), by = 2)]
  costs <- cv_results_vec[seq(from = 2, to = length(cv_results_vec), by = 2)]
  cost_best <- costs[which.max(accuracies)]
  acc_best <- max(accuracies)
  
  # get fit on full data
  print('getting final fit')
  image12_df <- rbind(image1_df, image2_df)
  fit_final <- svm(y ~ ., data = image12_df, cost = cost_best)
  
  cv_results <- list(accuracy = acc_best, cost_best = cost_best)
  
  print('done')
  return(list(fit = fit_final, 
              cv_results = cv_results))
}

get_svm_prediction <- function(fit, image3, smoothsv = TRUE){
  # takes in a svm fit, from the 'get_lasso_fit_2fold_cv' function above
  # and gets predictions on the test image (image3 here)
  # returns predictive probabilities, predicted classes, and accuracy
  
  # check if the fit was on smoothed or nonsmoothed features
  smooth_features <- 'NDAI_smoothed' %in% 
    names(fit$x.scale$`scaled:center`)

  # else the feature would be called 'NDAI' without the '_smoothed'
  
  # whether to take smoothed features or not
  if(smooth_features){
    feature_names <- c('NDAI_smoothed','CORR_smoothed','DF_smoothed')
  }else{
    feature_names <- c('NDAI','CORR','DF')
  }
  
  labeled_indx <- which(image3$label != 0)
  labeled_image <- image3[labeled_indx, ]
  
  y <- droplevels(labeled_image$label)
  
  X <- labeled_image[, feature_names]
  
  y_pred_class <- predict(fit, newdata = X) 

  # get predictive accuracy
  pred_accuracy <- mean(y_pred_class == y)
  
  return(list(pred_class = y_pred_class, 
              accuracy = pred_accuracy))
}

