#!/usr/bin/env Rscript

# requires packages: microbenchmark, Rcpp, RcppEigen

library(parallel)
library(Rcpp)
library(microbenchmark)


cxx_verbose <- FALSE

# Rcpp function
cppFunction(depends=c("RcppEigen"), plugins=c("openmp"), showOutput=cxx_verbose, '
    std::vector<int> eigen_rowMaximums(const Eigen::Map<Eigen::MatrixXi> &mat) {
        int nrows = mat.rows();
        int ncols = mat.cols();

        // create vector to store the result
        std::vector<int> maximums(nrows);

        // loop over rows
        #pragma omp parallel for
        for (int i = 0; i < nrows; i++) {
            // find the max for this row
            int mymax = -std::numeric_limits<int>::max();
            for (int j = 0; j < ncols; j++) {
                if (mat(i, j) > mymax) {
                    mymax = mat(i, j);
                }
            }
            maximums[i] = mymax;
        }

        return maximums;
    }'
)

# Rcpp function without explicit openmp
cppFunction(depends=c("RcppEigen"), plugins=c("openmp"), showOutput=cxx_verbose, '
    Rcpp::IntegerVector eigen_rowMaximumsRowwise(const Eigen::Map<Eigen::MatrixXi> &mat) {
        Eigen::VectorXi rowMaxs = mat.rowwise().maxCoeff();

        // make sure it is returned as a list, not a matrix
        Rcpp::IntegerVector rowMaxsVec = Rcpp::wrap(rowMaxs);
        rowMaxsVec.attr("dim") = R_NilValue;
        return rowMaxsVec;
    }'
)


# load data (depth is integer type)
load("sampdepth_max.rdata")
cat("depth", class(depth), typeof(depth), dim(depth), "\n")

# run the R function
rmax <- apply(depth, 1, max)
cat("rmax", class(rmax), typeof(rmax), length(rmax), "\n")

# run the Rcpp function
cmax <- eigen_rowMaximums(depth)
cat("cmax", class(cmax), typeof(cmax), length(cmax), dim(cmax), "\n")

# run the 2nd Rcpp function
cmax2 <- eigen_rowMaximumsRowwise(depth)
cat("cmax2", class(cmax2), typeof(cmax2), length(cmax2), dim(cmax2), "\n")

# check they give the same result
stopifnot(all.equal(rmax, cmax))
stopifnot(all.equal(rmax, cmax2))

# benchmark
microbenchmark(
        R=apply(depth, 1, max),
        EigenRow=eigen_rowMaximumsRowwise(depth),
        EigenOMP=eigen_rowMaximums(depth),
        times = 10
)
