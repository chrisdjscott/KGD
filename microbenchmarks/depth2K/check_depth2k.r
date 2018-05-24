#!/usr/bin/env Rscript

# requires packages: microbenchmark, Rcpp, RcppEigen and RcppArmadillo

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
depth2K <- function(depthvals)  1/2^depthvals   # convert depth to K value assuming binomial 

# eigen C++ function
cppFunction(depends=c("RcppEigen"), plugins=c("openmp"), showOutput=cxx_verbose, '
    Eigen::ArrayXXd eigen_depth2K(const Eigen::Map<Eigen::ArrayXXd> &A) {
        Eigen::ArrayXXd Aout = 1.0 / pow(2.0, A);
        return(Aout);
    }'
)

# eigen+openmp C++ function
cppFunction(depends=c("RcppEigen"), plugins=c("openmp"), showOutput=cxx_verbose, '
    Eigen::ArrayXXd eigomp_depth2K(const Eigen::Map<Eigen::ArrayXXd> &A) {
        Eigen::ArrayXXd Aout(A.rows(), A.cols());
        #pragma omp parallel for
        for (int j = 0; j < A.cols(); j++) {
            for (int i = 0; i < A.rows(); i++) {
                Aout(i, j) = 1.0 / pow(2.0, A(i, j));
            }
        }
        return Aout;
    }'
)

# Rcpp OpenMP C++ function
cppFunction(depends=c("Rcpp"), plugins=c("openmp"), showOutput=cxx_verbose, '
    Rcpp::NumericMatrix rcpp_depth2K(const Rcpp::NumericMatrix &A) {
        Rcpp::NumericMatrix Aout(A.rows(), A.cols());
        const long Asize = A.rows() * A.cols();
        #pragma omp parallel for
        for (long i = 0; i < Asize; i++) {
            Aout[i] = 1.0 / pow(2.0, A[i]);
        }
        return Aout;
    }'
)

# call functions and check output
rout <- depth2K(P0)
eigout <- eigen_depth2K(P0)
stopifnot(all.equal(rout, eigout))
rm(eigout)
eigompout <- eigomp_depth2K(P0)
stopifnot(all.equal(rout, eigompout))
rm(eigompout)
rcppout <- rcpp_depth2K(P0)
stopifnot(all.equal(rout, rcppout))
rm(rcppout)
rm(rout)

# microbenchmark
microbenchmark(R=depth2K(P0), Eigen=eigen_depth2K(P0), EigenOMP=eigomp_depth2K(P0), RcppOMP=rcpp_depth2K(P0), times = 10)
