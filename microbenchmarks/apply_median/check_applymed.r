#!/usr/bin/env Rscript

# requires packages: microbenchmark, Rcpp

library(parallel)
library(Rcpp)
library(microbenchmark)


cxx_verbose <- FALSE

# RcppArmadillo function
cppFunction(depends=c("RcppArmadillo"), plugins=c("openmp"), showOutput=cxx_verbose, '
    std::vector<double> arma_rowMedians(const arma::mat &depth) {
        arma::vec med = arma::median(depth, 1);
        return Rcpp::as<std::vector<double> >(Rcpp::wrap(med));
    }'
)

# same as above but different way of making sure the return argument is the same as from apply
cppFunction(depends=c("RcppArmadillo"), plugins=c("openmp"), showOutput=cxx_verbose, '
    Rcpp::NumericVector arma_rowMedians2(const arma::mat &depth) {
        arma::vec med = arma::median(depth, 1);
        Rcpp::NumericVector medvec = Rcpp::wrap(med);
        medvec.attr("dim") = R_NilValue;
        return medvec;
    }'
)

# RcppArmadillo function using OpenMP
cppFunction(depends=c("RcppArmadillo"), plugins=c("openmp"), showOutput=cxx_verbose, '
    Rcpp::NumericVector arma_rowMediansOMP(const arma::mat &depth) {
        // number of rows
        int nrows = depth.n_rows;

        // vector for storing the result
        Rcpp::NumericVector medvec(nrows);

        // loop over the rows
        #pragma omp parallel for
        for (int i = 0; i < nrows; i++) {
            // compute the median for this row
            arma::rowvec row = depth.row(i);
            medvec[i] = arma::median(row);
        }

        // make sure it is returned to R as a list, not matrix
        medvec.attr("dim") = R_NilValue;
        return medvec;
    }'
)

# Rcpp function using OpenMP and an Integer vector as the input (depth is integer)
cppFunction(depends=c("RcppArmadillo"), plugins=c("openmp"), showOutput=cxx_verbose, '
    Rcpp::IntegerVector arma_rowMediansOMPInt(const arma::imat &depth) {
        // number of rows
        int nrows = depth.n_rows;

        // vector for storing the result
        Rcpp::IntegerVector medvec(nrows);

        // loop over the rows
        #pragma omp parallel for
        for (int i = 0; i < nrows; i++) {
            // compute the median for this row
            arma::irowvec row = depth.row(i);
            medvec[i] = arma::median(row);
        }

        // make sure it is returned to R as a list, not matrix
        medvec.attr("dim") = R_NilValue;
        return medvec;
    }'
)

# load data
# if(outlevel > 4) sampdepth.med <<- apply(depth, 1, median)
# save(sampdepth, depth, file = "sampdepth_med.rdata")
load("sampdepth_med.rdata")
cat("depth", class(depth), typeof(depth), dim(depth), "\n")

# run the R function
rmed <- apply(depth, 1, median)
cat("rmed", class(rmed), typeof(rmed), length(rmed), "\n")

# run the Rcpp function
cmed <- arma_rowMedians(depth)
cat("cmed", class(cmed), typeof(cmed), length(cmed), dim(cmed), "\n")
stopifnot(all.equal(rmed, cmed))
rm(cmed)

# run second Rcpp function
cmed2 <- arma_rowMedians2(depth)
cat("cmed2", class(cmed2), typeof(cmed2), length(cmed2), dim(cmed2), "\n")
stopifnot(all.equal(rmed, cmed2))
rm(cmed2)

# run OpenMP armadillo function
cmed3 <- arma_rowMediansOMP(depth)
cat("cmed3", class(cmed3), typeof(cmed3), length(cmed3), dim(cmed3), "\n")
stopifnot(all.equal(rmed, cmed3))
rm(cmed3)

# run OpenMP armadillo function
cmed4 <- arma_rowMediansOMPInt(depth)
cat("cmed4", class(cmed4), typeof(cmed4), length(cmed4), dim(cmed4), "\n")
stopifnot(all.equal(rmed, cmed4))
rm(cmed4)
rm(rmed)

# benchmark
microbenchmark(
    R=apply(depth, 1, median),
    Arma=arma_rowMedians(depth),
    Arma2=arma_rowMedians2(depth),
    ArmaOMP=arma_rowMediansOMP(depth),
    ArmaOMPInt=arma_rowMediansOMPInt(depth),
    times = 10
)
