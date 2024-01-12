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

