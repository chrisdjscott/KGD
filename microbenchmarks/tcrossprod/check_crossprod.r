#!/usr/bin/env Rscript

# requires packages: microbenchmark, Rcpp, RcppEigen and RcppArmadillo

library(Rcpp)
library(microbenchmark)


cxx_verbose <- FALSE

# create matrix
dim0 <- 2581
dim1 <- 72993
P0 <- matrix(data = rexp(dim0 * dim1, rate = 10), nrow = dim0, ncol = dim1)
cat("DIM", dim(P0), "\n")

# create armadillo C++ function
cppFunction(depends=c("RcppArmadillo"), showOutput=cxx_verbose, '
    arma::mat arma_prod(arma::mat &m1) {
        arma::mat prod = m1 * trans(m1);
        return prod;
    }'
)

# create eigen C++ function
cppFunction(depends=c("RcppEigen"), plugins=c("openmp"), showOutput=cxx_verbose, '
    Eigen::MatrixXd eigen_prod(Eigen::Map<Eigen::MatrixXd> &m1) {
        Eigen::MatrixXd prod = m1 * m1.transpose();
        return prod;
    }'
)

# another Eigen function, using symmetry
# see https://cran.r-project.org/web/packages/RcppEigen/vignettes/RcppEigen-Introduction.pdf, fig.3
cppFunction(depends=c("RcppEigen"), plugins=c("openmp"), showOutput=cxx_verbose, '
    Eigen::MatrixXd eigen_prod2(Eigen::Map<Eigen::MatrixXd> &m1) {
        const int m(m1.rows());
        Eigen::MatrixXd prod(Eigen::MatrixXd(m, m).setZero().
                             selfadjointView<Eigen::Lower>().rankUpdate(m1));
        return prod;
    }'
)

# run all and check outputs match
res1 <- tcrossprod(P0)
res2 <- arma_prod(P0)
stopifnot(all.equal(res1, res2))
res2 <- eigen_prod(P0)
stopifnot(all.equal(res1, res2))
res2 <- eigen_prod2(P0)
stopifnot(all.equal(res1, res2))
rm(res1)
rm(res2)

# benchmark
microbenchmark(R=tcrossprod(P0), Arma=arma_prod(P0), Eigen=eigen_prod(P0), Eigen2=eigen_prod2(P0), times = 10)
