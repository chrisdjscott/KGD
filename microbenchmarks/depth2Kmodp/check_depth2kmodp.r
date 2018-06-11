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
depth2Kmodp <- function(depthvals, modp=0.5 ) {
    Kvals <- 0.5*modp^(depthvals-1)
    Kvals[which(depthvals==0)] <- 1
    Kvals
}

# Rcpp OpenMP C++ function
cppFunction(depends=c("Rcpp"), plugins=c("openmp"), showOutput=cxx_verbose, '
    Rcpp::NumericMatrix rcpp_depth2Kmodp(const Rcpp::NumericMatrix &depthvals, double modp = 0.5) {
        // create matrix for storing the result
        Rcpp::NumericMatrix result(depthvals.rows(), depthvals.cols());

        // size of the matrix
        const long size = depthvals.rows() * depthvals.cols();

        // loop over the elements in parallel
        #pragma omp parallel for
        for (long i = 0; i < size; i++) {
            double value = 0.5 * pow(modp, depthvals[i] - 1.0);
            result[i] = (value == 0) ? 1.0 : value;
        }
        return result;
    }'
)

# call functions and check output
rout <- depth2Kmodp(P0)
rcppout <- rcpp_depth2Kmodp(P0)
stopifnot(all.equal(rout, rcppout))

rout <- depth2Kmodp(P0, 0.7)
rcppout <- rcpp_depth2Kmodp(P0, 0.7)
stopifnot(all.equal(rout, rcppout))

rm(rcppout)
rm(rout)

# microbenchmark
microbenchmark(R=depth2Kmodp(P0), RcppOMP=rcpp_depth2Kmodp(P0), times = 10)
