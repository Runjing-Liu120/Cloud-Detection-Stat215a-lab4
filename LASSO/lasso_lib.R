get_fit_2fold_cv <- function(image1_features, image1_labels, 
                         image2_features, image2_labels, 
                         thresh = c(0.4, 0.6)){
  # return lasso model with lambda chosen by 2-fold CV
  # where the two folds are the two images
  # thresh are the probabilties below/above which we label 1 or -1
  # in between we label 0
  
  # fit to image 1
  fit1 <- glmnet(image1_features, image1_labels, family = "binomial")
  
  # predict on image 2
  image2_prediction_prob <- 
    predict(fit1, newx = image2_features, s = fit1$lambda, 
            type = 'response')
  image2_prediction <- 1.0 * (image2_prediction_prob > thresh[2]) + 
    -1.0 * (image2_prediction_prob < thresh[1]) 
  
  # get predictive accuracy
  acc1 <- colMeans(image2_prediction == image2_labels)
  
  # fit to image 2
  fit2 <- glmnet(image2_features, image2_labels, family = "binomial", 
                 lambda = fit1$lambda)
  
  # predict on image 1
  image1_prediction_prob <- 
    predict(fit2, newx = image1_features, s = fit2$lambda, 
            type = 'response')
  image1_prediction <- 1.0 * (image1_prediction_prob > thresh[2]) + 
    -1.0 * (image1_prediction_prob < thresh[1]) 
  
  # get predictive accuracy
  acc2 <- colMeans(image1_prediction == image1_labels)
  
  acc_avg <- 0.5 * (acc1 + acc2)
  lambda_best <- fit2$lambda[which.max(acc_avg)]
  
  # get fit on full data
  X <- rbind(image1_features, image2_features)
  y <- c(image1_labels, image2_labels)
  
  fit_final <- glmnet(X, y, family = "binomial", lambda = lambda_best)
  
  cv_results <- list(accuracy = acc_avg, lambda = fit1$lambda, 
                     lambda_best = lambda_best)
  
  return(list(fit = fit_final, 
              cv_results = cv_results))
}