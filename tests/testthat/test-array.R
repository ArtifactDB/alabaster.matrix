# This tests the saveObject generic for arrays.
# library(testthat); library(alabaster.matrix); source("setup.R"); source("test-array.R")

library(DelayedArray)
experiment <- "rnaseq"
assay <- "counts"

arr <- array(rpois(10000, 10), c(50, 20, 10))
dimnames(arr) <- list(
   paste0("GENE_", seq_len(nrow(arr))),
   letters[1:20],
   NULL
)

test_that("saveObject works as expected", {
    tmp <- tempfile()
    saveObject(arr, tmp)
    roundtrip <- readObject(tmp)
    expect_identical(normalizePath(BiocGenerics::path(roundtrip)), normalizePath(tmp))
    expect_identical(as.array(roundtrip), arr)

    # Works when it's officially integer.
    copy <- arr
    storage.mode(copy) <- "integer"
    tmp <- tempfile()
    saveObject(copy, tmp)
    expect_identical(as.array(readObject(tmp)), copy)

    # Works without dimnames.
    copy <- arr
    dimnames(copy) <- NULL
    tmp <- tempfile()
    saveObject(copy, tmp)
    expect_identical(as.array(readObject(tmp)), copy)
})

test_that("saveObject type optimization works as expected for integers", {
    # Small unsigned integers
    mat <- matrix(sample(255, 1000, replace=TRUE), 40, 25)
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)

    storage.mode(mat) <- "integer"
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)

    mat[100] <- NA
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)

    # Small signed integers
    mat <- matrix(sample(255, 1000, replace=TRUE) - 128, 40, 25)
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)

    storage.mode(mat) <- "integer"
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)

    mat[100] <- NA
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)

    # Large integers
    mat <- trunc(matrix(runif(1000, -1e6, 1e6), 40, 25))
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)

    storage.mode(mat) <- "integer"
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)

    mat[100] <- NA
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)
})

test_that("saveObject works as expected for floats", {
    mat <- matrix(rnorm(1000, -1e6, 1e6), 40, 25)
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)

    mat[100] <- NA
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)

    mat[101] <- NaN
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)

    mat[102] <- Inf
    mat[103] <- -Inf
    tmp <- tempfile()
    saveObject(arr, tmp)
    expect_identical(as.array(readObject(tmp)), arr)
})

test_that("saveObject works as expected for logicals", {
    mat <- matrix(rbinom(1000, 1, 0.5) == 1, 40, 25)
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)

    mat[100] <- NA
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)
})

test_that("saveObject works as expected for strings", {
    mat <- matrix(sample(LETTERS, 1000, replace=TRUE), 40, 25)
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)

    mat[100] <- NA
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)
})

test_that("saveObject works as expected for all-NA arrays", {
    mat <- matrix(NA, 50, 100)
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)

    storage.mode(mat) <- "integer"
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)

    storage.mode(mat) <- "double"
    tmp <- tempfile()
    saveObject(mat, tmp)
    expect_identical(as.matrix(readObject(tmp)), mat)
})

test_that("saveObject diverts correctly with pristine dense DelayedArrays", {
    x <- DelayedArray(arr)
    expect_true(isPristine(x))

    tmp <- tempfile()
    saveObject(x, tmp)
    expect_identical(as.array(readObject(tmp)), x@seed)

    # Also works if we ignore the pristine-ness.
    tmp <- tempfile()
    saveObject(x, tmp, DelayedArray.dispatch.pristine=FALSE)
    expect_identical(as.array(readObject(tmp)), x@seed)

    # Works with a custom override.
    old <- altSaveObjectFunction(function(...) {
        print("YAY")
        saveObject(...)
    })
    on.exit(altSaveObjectFunction(old), add=TRUE, after=FALSE)
    tmp <- tempfile()
    expect_output(saveObject(x, tmp), "YAY")
    expect_identical(as.array(readObject(tmp)), x@seed)

    altSaveObjectFunction(function(...) {
        stop("foobar")
    })
    tmp <- tempfile()
    expect_warning(saveObject(x, tmp), "foobar")
    expect_identical(as.array(readObject(tmp)), x@seed)

    setGeneric("stupidSave", function(x, path, ...) standardGeneric("stupidSave"))
    altSaveObjectFunction(stupidSave)
    tmp <- tempfile()
    expect_warning(saveObject(x, tmp), NA)
    expect_identical(as.array(readObject(tmp)), x@seed)
})

test_that("saveObject works correctly for unknown dense DelayedArrays", {
    x <- DelayedArray(new("SuperSeed", dim=c(100L, 50L)))
    tmp <- tempfile()
    saveObject(x, tmp)
    expect_identical(as.array(readObject(tmp)), as.matrix(x))
})

test_that("saveObject deduplicates dense arrays correctly", {
    skip_on_os("windows")

    mat <- matrix(runif(5000), 50, 100)
    tmp <- tempfile()
    session <- createDedupSession()

    saveObject(mat, tmp, array.dedup.session=session, array.dedup.action="symlink")
    expect_identical(as.matrix(readObject(tmp)), mat)

    tmp2 <- tempfile()
    saveObject(mat, tmp2, array.dedup.session=session, array.dedup.action="symlink")
    expect_identical(as.matrix(readObject(tmp2)), mat)
    expect_true(Sys.readlink(file.path(tmp2, "OBJECT")) != "") # Check that a symlink is actually formed.

    # Check that we get the same result from a pristine DelayedArray.
    tmp3 <- tempfile()
    saveObject(DelayedArray(mat), tmp3, array.dedup.session=session, array.dedup.action="symlink")
    expect_identical(as.matrix(readObject(tmp3)), mat)
    expect_true(Sys.readlink(file.path(tmp3, "OBJECT")) != "")

    # Deduplication works for DelayedArrays wrapping dense matrices.
    tmp4a <- tempfile()
    delayed <- DelayedArray(mat) * 2
    saveObject(delayed, tmp4a, array.dedup.session=session, array.dedup.action="symlink")
    expect_identical(as.matrix(readObject(tmp4a)), as.matrix(delayed))
    expect_true(Sys.readlink(file.path(tmp4a, "OBJECT")) == "")

    tmp4b <- tempfile()
    saveObject(delayed, tmp4b, DelayedArray.dispatch.pristine=FALSE, array.dedup.session=session, array.dedup.action="symlink")
    expect_identical(as.matrix(readObject(tmp4b)), as.matrix(delayed))
    expect_true(Sys.readlink(file.path(tmp4b, "OBJECT")) != "")
})

test_that("saveObject works correctly with dense block processing", {
    x <- DelayedArray(arr) * 1L
    expect_false(isPristine(x))

    for (bs in c(100, 200, 1000, 2000)) {
        tmp <- tempfile(fileext=".h5")
        local({
            oldh <- HDF5Array::getHDF5DumpChunkLength()
            olds <- getAutoBlockSize()
            HDF5Array::setHDF5DumpChunkLength(bs)
            setAutoBlockSize(bs * 4L)
            on.exit(HDF5Array::setHDF5DumpChunkLength(oldh))
            on.exit(setAutoBlockSize(olds), add=TRUE)
            saveObject(x, tmp)
        })

        roundtrip <- readObject(tmp)
        expect_identical(as.array(roundtrip), arr)
    }
})

test_that("reading dense arrays work with non-default NA placeholders", {
    tmp <- tempfile()
    saveObject(arr, tmp)
    first <- arr[1]

    library(rhdf5)
    local({ 
        fhandle <- H5Fopen(file.path(tmp, "array.h5"))
        on.exit(H5Fclose(fhandle), add=TRUE, after=FALSE)
        ghandle <- H5Gopen(fhandle, "dense_array")
        on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)
        dhandle <- H5Dopen(ghandle, "data")
        on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)
        alabaster.base::h5_write_attribute(dhandle, "missing-value-placeholder", first, scalar=TRUE)
    })

    ref <- arr
    ref[ref == first] <- NA
    expect_identical(ref, as.array(readObject(tmp)))
})

test_that("output grid creation works as expected", {
    grid <- alabaster.matrix:::create_output_grid(c(100L, 20L), c(5L, 4L), 20)
    expect_identical(grid@refdim, c(100L, 20L))
    expect_identical(grid@spacings, c(5L, 4L))

    grid <- alabaster.matrix:::create_output_grid(c(100L, 20L), c(5L, 4L), 30)
    expect_identical(grid@spacings, c(5L, 4L))

    grid <- alabaster.matrix:::create_output_grid(c(100L, 20L), c(5L, 4L), 60)
    expect_identical(grid@spacings, c(15L, 4L))

    grid <- alabaster.matrix:::create_output_grid(c(100L, 20L), c(5L, 4L), 400)
    expect_identical(grid@spacings, c(100L, 4L))

    grid <- alabaster.matrix:::create_output_grid(c(100L, 20L), c(5L, 4L), 800)
    expect_identical(grid@spacings, c(100L, 8L))

    grid <- alabaster.matrix:::create_output_grid(c(100L, 20L), c(7L, 4L), 200)
    expect_identical(grid@spacings, c(49L, 4L))

    grid <- alabaster.matrix:::create_output_grid(c(100L, 20L), c(7L, 4L), 400)
    expect_identical(grid@spacings, c(100L, 4L))

    grid <- alabaster.matrix:::create_output_grid(c(100L, 20L), c(7L, 3L), 30000)
    expect_identical(grid@spacings, c(100L, 20L))
})

test_that("saveObject works correctly with empty arrays", {
    x <- matrix(1, 100, 0)
    tmp <- tempfile()
    saveObject(x, tmp)
    roundtrip <- readObject(tmp)
    expect_identical(as.array(roundtrip), x)

    x2 <- DelayedArray(x) * 1
    tmp <- tempfile()
    saveObject(x2, tmp)
    roundtrip <- readObject(tmp)
    expect_identical(as.array(roundtrip), as.matrix(x2))
})

test_that("saveObject works correctly with VLS arrays", {
    tmp <- tempfile()
    dir.create(tmp)

    NR <- 10
    NC <- 12
    x <- matrix(sample(c("A", "BC", "DEFG", "HIJKL", "MNOP", "QRS", "TU", "V"), NR * NC, replace=TRUE), NR, NC)

    saveObject(x, file.path(tmp, "basic"), array.character.vls=TRUE)
    reloaded <- readObject(file.path(tmp, "basic")) 
    expect_identical(x, as.matrix(reloaded))

    y <- x
    y[2] <- NA
    saveObject(y, file.path(tmp, "with_missing"), array.character.vls=TRUE)
    reloaded <- readObject(file.path(tmp, "with_missing")) 
    expect_identical(y, as.matrix(reloaded))

    y <- x
    dimnames(y) <- list(letters[1:NR], seq_len(NC))
    saveObject(y, file.path(tmp, "named"), array.character.vls=TRUE)
    reloaded <- readObject(file.path(tmp, "named")) 
    expect_identical(y, as.matrix(reloaded))

    y <- x
    y[4] <- strrep("HIJKL", 100)
    saveObject(y, file.path(tmp, "auto"), array.character.vls=NULL)
    reloaded <- readObject(file.path(tmp, "auto")) 
    expect_identical(y, as.matrix(reloaded))
})
