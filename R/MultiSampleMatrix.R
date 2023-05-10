#' Multi-sample matrix
#'
#' Implements a multi-sample matrix as a delayed \code{\link{cbind}} of per-sample DelayedMatrix objects,
#' but with some options for extracting and listing the constituent matrices for each sample.
#' This allows \code{\link{stageObject}} to save multiple samples without aggregating them into a single file.
#'
#' @section Constructors:
#' \code{MultiSampleMatrixSeed(...)} accepts any number of named matrix-like objects and returns a MultiSampleMatrixSeed.
#' Each object corresponds to a block and should be named accordingly; names should be unique and non-empty.
#'
#' \code{MultiSampleMatrix(...)} accepts any number of named matrix-like objects and returns a MultiSampleMatrix.
#' Alternatively, a single MultiSampleMatrixSeed may be provided. 
#' 
#' @section Functions:
#' \code{listSamples(x)} will return a character vector of names of samples in a MultiSampleMatrix(Seed) object \code{x}.
#'
#' \code{extractSamples(x)} will return a named list of matrix-like objects,
#' corresponding to the per-sample matrices used to construct the MultiSampleMatrix(Seed) object \code{x}.
#'
#' @section Comments on usage:
#' The MultiSampleMatrixSeed is closely related to (and in fact, is a subclass of) the \linkS4class{DelayedAbind} class.
#' This means that we can leverage many of the \pkg{DelayedArray} methods for handling the delayed bind.
#' In theory, we could just use a DelayedAbind directly and save it with \pkg{chihaya} in \code{\link{stageObject}} (via \code{\link{preserveDelayedOperations}(TRUE)}).
#' However, this provides fewer opportunities for tracking and manipulating the samples.
#' It also saves the per-sample matrices into a single file, which eliminates possibilities for per-file deduplication and linking, e.g., with \code{\link{recycleHdf5Files}(TRUE)}.
#' 
#' @author Aaron Lun
#' @examples
#' first <- Matrix::rsparsematrix(10, 10, 0.1)
#' second <- Matrix::rsparsematrix(10, 20, 0.1)
#' mat <- MultiSampleMatrix(foo = first, bar = second)
#' mat
#'
#' listSamples(mat)
#' out <- extractSamples(mat)
#' lapply(out, dim)
#'
#' @aliases
#' MultiSampleMatrix
#' MultiSampleMatrix-class
#' MultiSampleMatrixSeed
#' MultiSampleMatrixSeed-class
#' DelayedArray,MultiSampleMatrixSeed-method
#' listSamples
#' extractSamples
#' 
#' @name MultiSampleMatrix
NULL

#' @export
#' @importFrom BiocGenerics cbind
MultiSampleMatrixSeed <- function(...) {
    collected <- list(...)
    sample.names <- names(collected)
    if (anyDuplicated(sample.names) || any(sample.names == "")) {
        stop("sample names must be unique and non-empty in a MultiSampleMatrixSeed")
    }

    combined <- do.call(cbind, lapply(collected, DelayedArray))
    new("MultiSampleMatrixSeed", combined@seed, samples = sample.names)
}

#' @export
#' @importFrom DelayedArray new_DelayedArray
setMethod("DelayedArray", "MultiSampleMatrixSeed", function(seed) new_DelayedArray(seed, Class="MultiSampleMatrix"))

#' @export
MultiSampleMatrix <- function(...) {
    collated <- list(...)
    if (length(collated) > 1 || !is(collated[[1]], "MultiSampleMatrixSeed")) {
        seed <- do.call(MultiSampleMatrixSeed, collated)
    } 
    DelayedArray(seed)
}

#' @export
listSamples <- function(x) {
    if (is(x, "DelayedMatrix")) {
        x <- x@seed
    }
    x@samples
}

#' @export
extractSamples <- function(x) {
    if (is(x, "DelayedMatrix")) {
        x <- x@seed
    }
    out <- x@seeds
    names(out) <- x@samples
    out
}
