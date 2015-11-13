
<<<<<<< Updated upstream
#' @title Calculate \eqn{L_q} distance 
=======
#' @title Calculate \eqn{L_q} distance of two vectors
#' @description Calculate \eqn{L_q} distance of two vectors
#' @param x A numeric vector. Missing values are allowed.
#' @param y A numeric vector. Missing values are allowed.
#' @param q An integer specifying the which norm to take the L-q distance of.
#' @return a scalar
#' @export
dist_q <- function(x, y, q = 2) {
  if (!is.numeric(x) || !is.numeric(y)) stop("Both x and y must be numeric.")
  if (!is.numeric(q) || length(q) != 1 || q != as.integer(q) || q < 1)
      stop("q must be an integer >= 1")

  x_obs <- !is.na(x)
  y_obs <- !is.na(y)
  ## return
  (sum(abs(x - y)^q, na.rm = TRUE) / sum(x_obs * y_obs))^(1/q)
}

#' @title Calculate \eqn{L_q} distance
>>>>>>> Stashed changes
#' @description Calculate \eqn{L_q} distance of all vectors in a matrix to a reference
#' vector.
#' @param x A numeric matrix Missing values are allowed.
#' @param ref An integer specifying the reference row.
#' @param q An integer specifying the which norm to take the L-q distance of.
#' @return a numeric vector of length \code{nrow(x) - 1}
#' @export
<<<<<<< Updated upstream
dist_q.matrix <- function(x, ref= 1L, q= 2) {
  if (!is.numeric(x) | !is.matrix(x)) stop("x must be a numeric matrix.")
  if (ref < 1 | ref > nrow(x) | ref %% 1 != 0) 
    stop("ref must be an integer in {1, nrow(x)}.")
  if (q < 1 | q %% 1 != 0) stop("q must be an integer >= 1")
  
  x_ref <- x[ref,]
  x_rest <- x[-ref,]
  
  return(.Call('imputation_dist_q_matrix', PACKAGE = 'imputation', x_ref, x_rest, q))
}


=======
dist_q.matrix <- function(x, ref = 1, q = 2) {
  if (!is.numeric(x) || !is.matrix(x)) stop("x must be a numeric matrix.")
  if (ref < 1 || ref > nrow(x)) stop("ref must be an integer in {1, .., nrow(x)}.")
  if (q < 1) stop("q must be an integer >= 1")

  x_ref <- x[ref,]
  x_rest <- x[-ref,]

  apply(x_rest, 1, dist_q, y = x_ref, q = q)
}



# @param x a data frame or matrix where each row represents a different record
impute_prelim <- function(x) {

  # 00. get some initial statistics on missingness.
  numMissing  <- sum(is.na(x))

  if(numMissing == 0) {
    return(list (numMissing = numMissing,
                 missing_rows_indices = NULL,
                 missing_cols_indices = NULL,
                 x_missing = NULL))
  }

  missing_rows_indices  <- which(apply(x, 1L, function(i) any(is.na(i))))
  missing_cols_indices  <- which(apply(x, 2L, function(i) any(is.na(i))))

  # 01. add a row identifier to x[, <missing columns>]
  x_missing  <- cbind(1:nrow(x),x)[missing_rows_indices,,drop = FALSE]

  # 02. return
  list (numMissing = numMissing,
        missing_rows_indices = missing_rows_indices,
        missing_cols_indices = missing_cols_indices,
        x_missing = x_missing)
}

>>>>>>> Stashed changes
### pairwise tests of a dataset's columns for equal variance
var_tests <- function(x, bonf = TRUE, conf.level = 0.95) {
    stopifnot(is.numeric(p <- ncol(x)), p >= 1)
    ntests <- choose(p,2)
    ret <- matrix(NA, ncol = p, nrow = p)
    for (i in 1:(p-1)) {
        for (j in (i+1):p) {
            ## fill lower triangular
            ret[j,i] <- var.test(x[,i], x[,j], conf.level = conf.level)$p.val
        }
    }
    alph <- 1-conf.level
    ## return   unequal_scaled_vars =
    calc_i_j(ret, alpha = if(bonf) alph/ntests else alph)
}


calc_i_j <- function(mat, alpha = 0.05) {
    n <- which(mat < alpha)
    if (length(n) > 0) {
        d <- dim(mat)
        i <- n %% d[1]; i[i == 0] <- d[1]
        j <- ceiling(n / d[2])
        data.frame(i = i, j = j, alpha = mat[n])[order(i), ]
    } ## else NULL
}
