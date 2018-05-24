#!/usr/bin/env Rscript

# requires packages: microbenchmark, Rcpp

library(Rcpp)
library(microbenchmark)


cxx_verbose <- FALSE

# load data
load("P01_before.rdata")
cat("P0", class(P0), typeof(P0), "\n")
cat("P1", class(P1), typeof(P1), "\n")
cat("depth", class(depth), typeof(depth), "\n")
cat("indsubset", class(indsubset), typeof(indsubset), "\n")
cat("snpsubset", class(snpsubset), typeof(snpsubset), "\n")
cat("usegeno", class(usegeno), typeof(usegeno), "\n")

# R function
dsub <- depth[indsubset, snpsubset]
r_assign <- function(usegeno, dsub) {
    P0[!usegeno | dsub < 2] <<- 0
    P1[!usegeno | dsub < 2] <<- 0
    genon01[dsub < 2] <<- 0
}


# Rcpp function
cppFunction(depends=c("Rcpp"), plugins=c("openmp"), showOutput=cxx_verbose, '
    void rcpp_assign(Rcpp::NumericMatrix &P0, Rcpp::NumericMatrix &P1, Rcpp::NumericMatrix &genon01,
                    const Rcpp::LogicalMatrix &usegeno, const Rcpp::NumericMatrix &dsub) {
        long size = P0.rows() * P0.cols();
        #pragma omp parallel for
        for (long i = 0; i < size; i++) {
            if (dsub[i] < 2.0) {
                P0[i] = 0.0;
                P1[i] = 0.0;
                genon01[i] = 0.0;
            }
            else if (!usegeno[i]) {
                P0[i] = 0.0;
                P1[i] = 0.0;
            }
        }
        return;
    }'
)

# run R function (updates P0, P1 and genon01 directly)
r_assign(usegeno, dsub)
rP0 <- P0
rP1 <- P1
rGenon01 <- genon01

# load original P0, P1, genon01
rm(P0)
rm(P1)
rm(genon01)
load("P01_before.rdata")

# should differ now
if (isTRUE(all.equal(rP0, P0))) {
    stop('R function should have changed rP0')
}

# run Rcpp function (updates P0, P1, genon01 directly)
rcpp_assign(P0, P1, genon01, usegeno, dsub)

stopifnot(all.equal(rP0, P0))
stopifnot(all.equal(rP1, P1))
stopifnot(all.equal(rGenon01, genon01))

cppP0 <- P0
cppP1 <- P1

rm(P0)
rm(P1)

# load saved data from run to check correct
load("P01_after.rdata")
stopifnot(all.equal(P0, cppP0))
stopifnot(all.equal(P1, cppP1))
cat("checks all pass\n")

rm(rP0)
rm(rP1)
rm(rGenon01)
rm(cppP0)
rm(cppP1)

# benchmark
microbenchmark(R=r_assign(usegeno, dsub), Rcpp=rcpp_assign(P0, P1, genon01, usegeno, dsub), times = 100)
