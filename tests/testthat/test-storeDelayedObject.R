# This tests the behavior of the various HDF5-based seeds.
# library(testthat); library(alabaster.matrix); source("test-storeDelayedObject.R")

library(DelayedArray)
library(rhdf5)

saveDelayed <- function(x) {
    tmp <- tempfile()
    dir.create(tmp)
    upath <- file.path(tmp, "foo.h5")

    fhandle <- H5Fcreate(upath)
    on.exit(H5Fclose(fhandle))
    storeDelayedObject(x@seed, fhandle, "FOO")
    upath
}

loadDelayed <- function(path, ...) {
    fhandle <- H5Fopen(path, "H5F_ACC_RDONLY")
    on.exit(H5Fclose(fhandle))
    reloadDelayedObject(fhandle, "FOO", ...)
}

#######################################################
#######################################################

test_that("ConstantArrays are saved correctly", {
    # Behaves correctly for logical values.
    X <- ConstantArray(dim=c(12, 7))
    temp <- saveDelayed(X)

    out <- loadDelayed(temp)
    expect_identical(X, out)

    # Behaves correctly for string values.
    X <- ConstantArray(dim=c(12, 7), value="foo")
    temp <- saveDelayed(X)

    out <- loadDelayed(temp)
    expect_identical(X, out)

    # Behaves correctly for numeric values.
    X <- ConstantArray(dim=c(12, 7), value=2L)
    temp <- saveDelayed(X)

    out <- loadDelayed(temp)
    expect_identical(X, out)
})

test_that("ConstantArrays are still saved correctly after some deep nesting", {
    X <- ConstantArray(dim=c(12, 7), value=23)
    Y <- DelayedArray(matrix(runif(60), nrow=12))
    Z <- cbind(X, Y)

    temp <- saveDelayed(Z)
    out <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(Z, out)
})

test_that("ConstantArrays behave correctly with NAs", {
    X <- ConstantArray(dim=c(12, 7), value=NA)
    temp <- saveDelayed(X)
    out <- loadDelayed(temp)
    expect_identical(X, out)

    # Trying a non-Default NA.
    X <- ConstantArray(dim=c(12, 7), value=1.2)
    temp <- saveDelayed(X)

    library(rhdf5)
    (function() {
        fhandle <- H5Fopen(temp)
        on.exit(H5Fclose(fhandle), add=TRUE)
        ghandle <- H5Gopen(fhandle, "FOO")
        on.exit(H5Gclose(ghandle), add=TRUE)
        dhandle <- H5Dopen(ghandle, "value")
        on.exit(H5Dclose(dhandle), add=TRUE)
        h5writeAttribute(1.2, dhandle, "missing_placeholder")
    })()

    out <- loadDelayed(temp)
    expect_identical(ConstantArray(c(12, 7), value=NA_real_), out)
})

#######################################################
#######################################################

test_that("DelayedAbind works along rows", {
    X <- DelayedArray(matrix(runif(60), ncol=20))
    Y <- DelayedArray(matrix(runif(100), ncol=20))
    Z <- rbind(X, Y)
    temp <- saveDelayed(Z)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(Z, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedAbind")
})

test_that("DelayedAbind works along columns", {
    A <- DelayedArray(matrix(runif(60), nrow=20))
    B <- DelayedArray(matrix(runif(100), nrow=20))
    C <- DelayedArray(matrix(runif(200), nrow=20)) # throwing in another dataset for some variety.
    Z <- BiocGenerics::cbind(A, B, C)
    temp <- saveDelayed(Z)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(Z, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedAbind")
})

test_that("DelayedAbind works for 3D arrays", {
    A <- DelayedArray(array(runif(100), c(10, 5, 4)))
    B <- DelayedArray(array(runif(100), c(10, 5, 4)))
    Z <- arbind(A, B)
    temp <- saveDelayed(Z)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(Z, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedAbind")
})

#######################################################
#######################################################

test_that("DelayedAperm works along rows", {
    X <- DelayedArray(matrix(runif(100), ncol=20))
    Z <- t(X)
    temp <- saveDelayed(Z)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(Z, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedAperm")
})

test_that("DelayedAperm works for 3D arrays", {
    A <- DelayedArray(array(runif(100), c(10, 5, 4)))
    perm <- c(3L, 1L, 2L)
    Z <- aperm(A, perm)
    temp <- saveDelayed(Z)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(Z, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedAperm")
})

#######################################################
#######################################################

test_that("DelayedNaryIsoOp works as expected (arithmetic)", {
    X <- DelayedArray(matrix(runif(100), ncol=5))
    Y <- DelayedArray(matrix(runif(100), ncol=5))
    Z <- X * Y
    temp <- saveDelayed(Z)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(Z, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedNaryIsoOp")
})

test_that("DelayedNaryIsoOp works as expected (comparison)", {
    X <- DelayedArray(matrix(rpois(100, 5), ncol=5))
    Y <- DelayedArray(matrix(rpois(100, 2), ncol=5))
    Z <- X > Y
    temp <- saveDelayed(Z)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(Z, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedNaryIsoOp")
})

test_that("DelayedNaryIsoOp works as expected (logic)", {
    X <- DelayedArray(matrix(rbinom(100, 1, 0.5) != 0, ncol=5))
    Y <- DelayedArray(matrix(rbinom(100, 1, 0.5) != 0, ncol=5))
    Z <- X | Y
    temp <- saveDelayed(Z)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(Z, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedNaryIsoOp")
})

test_that("DelayedNaryIsoOp works for 3D arrays", {
    A <- DelayedArray(array(runif(100), c(10, 5, 4)))
    B <- DelayedArray(array(runif(100), c(10, 5, 4)))
    Z <- A <= B
    temp <- saveDelayed(Z)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(Z, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedNaryIsoOp")
})

test_that("DelayedNaryIsoOp works with multiple seeds", {
    X <- DelayedArray(matrix(rpois(100, 5), ncol=5))
    Y <- DelayedArray(matrix(rpois(100, 2), ncol=5))
    Z <- DelayedArray(matrix(rbinom(100, 1, 0.5) == 0, ncol=5))
    AA <- X - Y + Z
    temp <- saveDelayed(AA)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(AA, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedNaryIsoOp")
})

#######################################################
#######################################################

test_that("DelayedSetDimnames works as expected (colnames only)", {
    Z <- DelayedArray(matrix(runif(100), ncol=20))
    colnames(Z) <- LETTERS[1:20]
    temp <- saveDelayed(Z)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(Z, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedSetDimnames")
})

test_that("DelayedSetDimnames works as expected (rownames only)", {
    Z <- DelayedArray(matrix(runif(100) < 0.1, ncol=20))
    rownames(Z) <- letters[1:5]
    temp <- saveDelayed(Z)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(Z, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedSetDimnames")
})

test_that("DelayedSetDimnames works as expected (both sets of names)", {
    Z <- DelayedArray(matrix(rpois(100, 5), ncol=20))
    dimnames(Z) <- list(letters[1:5], LETTERS[1:20])
    temp <- saveDelayed(Z)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(Z, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedSetDimnames")
})

#######################################################
#######################################################

test_that("DelayedSubassign works when all indices are supplied", {
    X <- DelayedArray(matrix(runif(100), ncol=20))
    X[1:2,c(1, 10, 20)] <- matrix(-runif(6), ncol=3)
    temp <- saveDelayed(X)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(X, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedSubassign")
})

test_that("DelayedSubassign works when only one index is supplied", {
    X <- DelayedArray(matrix(runif(100), ncol=20))
    X[c(1, 5), ] <- matrix(-rpois(2*ncol(X), 12), ncol=ncol(X))
    temp <- saveDelayed(X)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(X, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedSubassign")
})

test_that("DelayedSubassign works when the replacement is a DelayedArray", {
#    X <- DelayedArray(matrix(rbinom(100, 1, 0.5) == 0, ncol=20))
#    X[1:2,3:5] <- DelayedArray(matrix(-runif(6), ncol=3)) + 1
#    temp <- saveDelayed(X)
#
#    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
#    expect_identical(X, roundtrip)
#    expect_s4_class(roundtrip@seed, "DelayedSubassign")
})

#######################################################
#######################################################

test_that("DelayedSubset works when all indices are supplied", {
    X <- DelayedArray(matrix(runif(100), ncol=20))
    Y <- X[1:2,3:5]
    temp <- saveDelayed(Y)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(Y, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedSubset")

    # Trying with out-of-order indices.
    Y <- X[c(1,3,5),c(2,8,6,4)]
    temp <- saveDelayed(Y)
    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(Y, roundtrip)
})

test_that("DelayedSubset works when only one index is supplied", {
    X <- DelayedArray(matrix(runif(100), ncol=20))
    Y <- X[,c(20, 1, 3, 15, 9)] 
    temp <- saveDelayed(Y)

    roundtrip <- loadDelayed(temp, custom.takane.realize=TRUE)
    expect_identical(Y, roundtrip)
    expect_s4_class(roundtrip@seed, "DelayedSubset")
})
