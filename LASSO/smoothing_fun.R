library(FNN)

smooth_features <- function(coord, feature_vec, k = 64){
  # coord: the location of the pixels n x 2 matrix of coordinates
  # feature_vec: vector of predicted features to be smoothed 
  # k: number of neighbors to consider
  
  knn_object <- knn.reg(coord, coord, feature_vec)
  
  return(knn_object$pred)
}

smooth_all_features <- function(X, coord, k = 64){
  # a wrapper on the function "smooth_features" above 
  # appends columns to X with features that are smoothed with KNN
  # coord is the location of the pixels (ie how we determine neighbors)
  
  feature_names <- colnames(X)
  features <- X[, feature_names]

  for(i in 1:length(feature_names)){
    features[[paste(feature_names[i], '_smoothed', sep = '')]] <- 
      smooth_features(coord, features[, i])
  }
  
  return(features)
}
  
  
add_interaction_terms <- function(X, deg = 2){
  # takes features, add interaction terms of order "deg"
  feature_names <- colnames(X)
  features <- X[, feature_names]
  n_features <- length(feature_names)
  for(i in 1:n_features){
    for(j in 1:i){
      features[[paste(feature_names[i], feature_names[j], sep = '_')]] <- 
        X[, i] * X[, j]
    }
  }
  
  return(features)
}
  
  
# DELETEME ----------------------------------------
# smooth_labels <- function(coord, labels, radius = 2, weight = 0.5){
#   # coordinates: matrix of two columns, the first being x and the second being y
#   # labels: vector of predicted labels. 
#   # weight is the weight given to the original probability; 
#   # the neighbors get the remaining weight
#   # returns a smoothed label, averaged over labels at most 'radius' apart in L1 distance
#   
#   coord_mat <- as.matrix(coord)
#   
#   stopifnot(dim(coord)[2] == 2)
#   stopifnot(dim(coord)[1] == length(labels))
#   
#   n_pixels <- length(labels)
#   labels_smoothed <- rep(0, n_pixels)
#   
#   for(i in 1:n_pixels){
#     coord_i <- coord_mat[i, ]
#     neighbors_indx <- which(colSums(abs(t(coord_mat) - coord_i)) <= radius & 
#                               colSums(abs(t(coord_mat) - coord_i)) != 0)
#     
#     labels_smoothed[i] <- weight * labels[i] + (1 - weight) * mean(labels[neighbors_indx])
#     
#   }
#   
#   return(labels_smoothed)
# }
