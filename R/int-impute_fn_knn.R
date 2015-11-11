
# @description wrapper to impute_fn_knn for all observations and all columns.
# This function is not parallelized
# @param x_missing From impute_prelim(...)$x_missing
# @param x_complete The complete data matrix x
# @param k ... input to kNN_impute
# @param q ... input to kNN_impute
# @param sigma ... calculated inside kNN_impute
# @param verbose if \code{TRUE} print status updates
impute_fn_knn_all.nonPar <- function(x_missing, x_complete, k, q, sigma,
                                     verbose) {
  # impute row-by-row -- non parallel
  x_missing_imputed <- t(apply(x_missing, 1, function(i) {
    rowID = as.numeric(i[1])
    i_original = unlist(i[-1])
    x_comp_rowID <- which(as.integer(rownames(x_complete)) == rowID)
    # verbose option
    if(verbose) {print(paste("Imputing row", rowID, sep=" "))}
    missing_cols <- which(is.na(x_complete[x_comp_rowID,]))
    
    # calculate distances
    
    distances <- dist_q.matrix(rbind(x_complete[x_comp_rowID, ], x_complete[-x_comp_rowID,]), 
                               ref= 1L, q= q)
    
    # within the given row, impute by column
    imputed_values <- unlist(lapply(missing_cols, function(j, distances) {
      # which neighbors have data on column j?
      neighbor_indices = which(!is.na(x_complete[,j]))
      # impute
      return(impute_fn_knn(x_complete[neighbor_indices, j], 
                           distances[neighbor_indices + ifelse(x_comp_rowID < neighbor_indices, 0, -1)], 
                           k=k, sigma= sigma))
    }, distances= distances))
    i_original[missing_cols] <- imputed_values
    return(i_original)
  }))
  return(x_missing_imputed)
}




# @description wrapper to impute_fn_knn for all observations and all columns.
# This function is parallelized
# @param x_missing From impute_prelim(...)$x_missing
# @param x_complete The complete data matrix x
# @param k ... input to kNN_impute
# @param q ... input to kNN_impute
# @param sigma ... calculated inside kNN_impute
# @param leave_cores How many cores do you wish to leave open to other processing?
impute_fn_knn_all.Par <- function(x_missing, x_complete, k, q, sigma,
                                  leave_cores) { 
  ### [AW 10/20] resolve edge case when nnodes > nrow(x_missing)
  nnodes <- min(nrow(x_missing), detectCores() - leave_cores)
  cl <- makeCluster(nnodes)
  
  # impute row-by-row -- parallel 
  x_missing_imputed <- parRapply(cl= cl, x_missing, function(i, x_complete, sigma) {
    rowID = as.numeric(i[1])
    i_original = unlist(i[-1])
    x_comp_rowID <- which(as.integer(rownames(x_complete)) == rowID)
    missing_cols <- which(is.na(x_complete[x_comp_rowID,]))
    
    # calculate distances
    distances <- dist_q.matrix(x=rbind(x_complete[x_comp_rowID, ], x_complete[-x_comp_rowID,]), 
                               ref= 1L, q= q)
    
    # within the given row, impute by column
    imputed_values <- unlist(lapply(missing_cols, function(j, distances) {
      # which neighbors have data on column j?
      neighbor_indices = which(!is.na(x_complete[,j]))
      # impute
      return(impute_fn_knn(x_complete[neighbor_indices, j], 
                           distances[neighbor_indices + ifelse(x_comp_rowID < neighbor_indices, 0, -1)], 
                           k=k, sigma= sigma))
    }, distances= distances))
    i_original[missing_cols] <- imputed_values
    return(i_original)
  }, x_complete= x_complete, sigma= sigma)
  
  stopCluster(cl)
  x_missing_imputed <- matrix(x_missing_imputed, nrow= dim(x_missing)[1],
                              ncol= dim(x_missing)[2] - 1, byrow= TRUE)
  return(x_missing_imputed)
}



# @description wrapper to impute_all_knn (C++) for all observations and all columns.
# @param x_missing From impute_prelim(...)$x_missing
# @param x_complete The complete data matrix x
# @param k ... input to kNN_impute
# @param q ... input to kNN_impute
# @param sigma ... calculated inside kNN_impute
# @param leave_cores How many cores do you wish to leave open to other processing?

# [AW 11/11/2015] -- not needed. The c++ version of impute_all_knn is NOT Faster than the R version
# impute_fn_knn_all <- function(x_missing, x_complete, k, q, sigma,
#                               verbose, leave_cores= NULL) {
#   
#   if (is.null(leave_cores)) {
#     x_missing_imputed <- .Call('imputation_impute_all_knn', PACKAGE = 'imputation', 
#                                x_missing, x_complete, k, q, sigma, verbose)
#   } else {
#     nnodes <- min(nrow(x_missing), detectCores() - leave_cores)
#     cl <- makeCluster(nnodes)
#     x_missing_imputed <- do.call("rbind", clusterApply(cl, 
#       x= parallel:::splitRows(x_missing, nnodes), 
#       fun= impute_all_knn,
#       x_complete = x_complete, k= k, q= q, sigma= sigma, verbose= verbose))
#     stopCluster(cl)
#   }
#   return(x_missing_imputed)
# }