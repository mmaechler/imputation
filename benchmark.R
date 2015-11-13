<<<<<<< Updated upstream


library(imputation)
library(microbenchmark)

dat_list <- list(x1= matrix(rnorm(300), nrow= 10),
                 x2= matrix(rnorm(3000), nrow= 100),
                 x3= matrix(rnorm(30000), nrow= 1000),
                 x4= matrix(rnorm(300000), nrow= 10000),
                 x5= matrix(rnorm(3000000), nrow= 100000))

dat_list <- lapply(dat_list, function(l) {l[l>1.25] <- NA; return(l)})

# 01. Non-parallel, no canopies
#------------------------------------------------
(m1 <- microbenchmark(
  n10= kNN_impute(x= dat_list[[1]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                  parallel= FALSE),
  n100= kNN_impute(x= dat_list[[2]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                  parallel= FALSE), 
  n1000= kNN_impute(x= dat_list[[3]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                  parallel= FALSE), 
  n10000= kNN_impute(x= dat_list[[4]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                  parallel= FALSE), times= 20L))


# 02. parallel, no canopies
#------------------------------------------------
(m2 <- microbenchmark(
  n10= kNN_impute(x= dat_list[[1]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                  parallel= TRUE, leave_cores= 1),
  n100= kNN_impute(x= dat_list[[2]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                   parallel= TRUE, leave_cores= 1), 
  n1000= kNN_impute(x= dat_list[[3]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                    parallel= TRUE, leave_cores= 1), 
  n10000= kNN_impute(x= dat_list[[4]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                     parallel= TRUE, leave_cores= 1), times= 20L))

# 03. parallel, canopies vs no-canopies
#------------------------------------------------
(m3 <- microbenchmark(
  n10k_no_can= kNN_impute(x= dat_list[[4]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                  parallel= TRUE, leave_cores= 1),
  n100k_no_can= kNN_impute(x= dat_list[[5]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                   parallel= TRUE, leave_cores= 1), 
  n10k_can= kNN_impute(x= dat_list[[4]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                    parallel= TRUE, leave_cores= 1, n_canopies= 100), 
  n100k_can= kNN_impute(x= dat_list[[5]], k=3, q= 4, verbose= FALSE, check_scale= FALSE,
                     parallel= TRUE, leave_cores= 1, n_canopies= 1000), times= 10L))


# save.image("knn_timing.Rdata")
=======
imputation.benchmark.random <- function(numRow = 100, numCol = 100, numMissing = 50,
                                        imputation.fn = NULL, ...)
{
    if(!is.function(imputation.fn))
        stop("'imputation.fn' must be an imputation R function")

    missingX  <- sample.int(numRow, numMissing, replace = TRUE)
    missingY  <- sample.int(numCol, numMissing, replace = TRUE)

    x.missing <- x <- matrix(rnorm(numRow * numCol), numRow, numCol)
    x.missing[cbind(missingX, missingY)] <- NA
    x.imputed  <- imputation.fn(x.missing, ...)

    SE  <- mapply(function(m.x,m.y) ((x[m.x, m.y] - x.imputed[m.x, m.y]) / x[m.x, m.y])^2,
                  missingX, missingY)
    ## return
    list(data = x,
         missing = x.missing,
         imputed = x.imputed,
         rmse = sqrt(mean(SE))
    )
}

imputation.benchmark.ts <- function(numTS = 100, TSlength = 100, numMissing = 50,
                                    imputation.fn = NULL, ...)
{
    if(!is.function(imputation.fn))
        stop("'imputation.fn' must be an imputation R function")

    missingX  <- sample.int(numTS, numMissing, replace = TRUE)
    missingY  <- sample.int(TSlength, numMissing, replace = TRUE)

    x.missing <- x <- t(sapply(1:numTS, function(i) {
        i01  <- rbinom(1, prob= 1/2)
        ## Need to be careful to only generate time series that are stationary
        as.vector(
            if(i01) {
                arima.sim(n = TSlength, list(ar = c(0.8, -0.5), ma = c(-0.23, 0.25)) )
            } else if(rand > 0) {
                arima.sim(n = TSlength, list(ar = c(1, -0.5), ma = c(-.4)) )
            })
    }))
    x.missing[cbind(missingX, missingY)] <- NA
    x.imputed  <- imputation.fn(x.missing, ...)

    SE  <- mapply(function(m.x,m.y) ((x[m.x, m.y] - x.imputed[m.x, m.y]) / x[m.x, m.y])^2,
                  missingX, missingY)
    ## return
    list(data = x,
         missing = x.missing,
         imputed = x.imputed,
         rmse = sqrt(mean(SE)))
}
>>>>>>> Stashed changes
