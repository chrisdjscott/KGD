#!/usr/bin/env Rscript

# requires packages: microbenchmark, Rcpp

library(Rcpp)
library(microbenchmark)

# view compiler output
cxx_verbose <- FALSE

# create matrix
dim0 <- 2581
dim1 <- 72993
P0 <- matrix(data = rexp(dim0 * dim1, rate = 10), nrow = dim0, ncol = dim1)
cat("DIM", dim(P0), "\n")

# R function
depth2Kbb <- function(depthvals, alph=Inf) {
    # convert depth to K value assuming beta-binomial with parameters alpha=beta=alph. Inf gives binomial
    if (alph==Inf) 1/2^depthvals else beta(alph,depthvals+alph)/beta(alph,alph)
}

# Rcpp OpenMP C++ function
cppFunction(depends=c("Rcpp"), plugins=c("openmp"), showOutput=cxx_verbose, '
    Rcpp::NumericMatrix rcpp_depth2Kbb(const Rcpp::NumericMatrix &depthvals, double alph = 9999) {
        // create matrix for storing the result
        Rcpp::NumericMatrix result(depthvals.rows(), depthvals.cols());

        // size of the matrix
        const long size = depthvals.rows() * depthvals.cols();

        // loop over the elements in parallel
        #pragma omp parallel for
        for (long i = 0; i < size; i++) {
            double value = R::beta(alph,depthvals[i] + alph) / R::beta(alph,alph);
            result[i] = value;
        }

        return result;
    }'
)

# call functions and check output
cat("Calling R function...\n")
rout <- depth2Kbb(P0, 0.7)
cat("Calling C++ function...\n")
rcppout <- rcpp_depth2Kbb(P0, 0.7)
cat("Comparing results...\n")
stopifnot(all.equal(rout, rcppout))
cat("Results match!\n")
rm(rcppout)
rm(rout)

# microbenchmark
microbenchmark(R=depth2Kbb(P0), RcppOMP=rcpp_depth2Kbb(P0), times = 5)
