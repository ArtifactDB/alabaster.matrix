#' Reloaded \pkg{alabaster} array 
#'
#' An array that was reloaded from disk by the \code{\link{readObject}} function.
#' This allows methods to refer to the existing on-disk representation by inspecting the \code{path}.
#' For example, \code{\link{saveObject}} can just copy/link to the files instead of repeating the saving process.
#'
#' @param path String containing a path to the directory with the on-disk array representation.
#' Alternatively an existing ReloadedArraySeed, which is returned without modification.
#' @param seed Contents of the loaded array, e.g., as an ordinary R array, a \linkS4class{DelayedArray} or a sparse matrix.
#' If \code{NULL}, this is obtained by calling \code{\link{readObject}}.
#' @param ... Further arguments to pass to \code{\link{readObject}} when \code{seed=NULL}.
#'
#' @return
#' For the constructors, an instance of the \linkS4class{ReloadedArraySeed} or \linkS4class{ReloadedArray}.
#'
#' @details
#' The ReloadedArraySeed is a subclass of the \linkS4class{WrapperArraySeed} and will just forward all operations to the underlying \code{seed}.
#' Its main purpose is to track the \code{path} that was originally used to generate \code{seed}, which enables optimizations for methods that need to operate on the files.
#'
#' One obvious optimization is the specialization of \code{\link{saveObject}} on ReloadedArray instances.
#' Instead of loading the array data back into the R session and saving it again, the \code{saveObject} method can just link or copy the existing files.
#' This behavior is controlled by the optional \code{ReloadedArray.reuse.files} option in the \code{saveObject} method, which can be one of:
#' \itemize{
#' \item \code{"copy"}: copy the files from the original directory (as stored in the ReloadedArray object) to the new \code{path} specified in \code{saveObject}.
#' \item \code{"link"}: create a hard link from the files in the original directory to the new \code{path}.
#' If this fails, we silently fall back to a copy.
#' This mode is the default approach.
#' \item \code{"symlink"}: create a symbolic link from the files in the original directory to the new \code{path}.
#' Each symbolic link refers to an absolute path in the original directory, which is useful when the contents of \code{path} might be moved (but the original directory will not).
#' \item \code{"relsymlink"}: create a symbolic link from the files in the original directory to the new \code{path}.
#' Each symbolic link refers to an relative path to its corresponding file in the original directory,
#' which is useful when both \code{path} and the original directory are moved together, e.g., as they are part of the same parent object like a SummarizedExperiment.
#' \item \code{"none"}: ignore existing files and just save the contents by calling \code{"\link{saveObject,DelayedArray-method}"}.
#' }
#' 
#' @examples
#' arr <- array(rpois(10000, 10), c(50, 20, 10))
#' dir <- tempfile()
#' saveObject(arr, dir)
#' obj <- readArray(dir)
#' obj
#' DelayedArray::showtree(obj)
#' 
#' @name ReloadedArraySeed
#' @aliases
#' ReloadedArraySeed-class
#' ReloadedArray-class
#' ReloadedMatrix-class
#' DelayedArray,ReloadedArraySeed-method
#' matrixClass,ReloadedArray-method
#' coerce,ReloadedArray,ReloadedMatrix-method
#' coerce,ReloadedMatrix,ReloadedArray-method
#' path,ReloadedArraySeed-method
#' saveObject,ReloadedArray-method
#'
#' @export
ReloadedArraySeed <- function(path, seed=NULL, ...) {
    if (is(path, "ReloadedArraySeed")) {
        return(path)
    }
    if (is.null(seed)) {
        seed <- readObject(path, ...)
    }
    while (is(seed, "DelayedArray")) {
        seed <- seed@seed
    }

    # Need to obtain an absolute path in order for this to be safe in
    # saveObject(), possibly after changes to the working directory has.
    new("ReloadedArraySeed", path=normalizePath(path, mustWork=TRUE), seed=seed)
}

#' @export
#' @rdname ReloadedArraySeed
ReloadedArray <- function(path, seed=NULL, ...) {
    DelayedArray(ReloadedArraySeed(path, seed=seed, ...))
}

#' @export
#' @importFrom DelayedArray DelayedArray new_DelayedArray
setMethod("DelayedArray", "ReloadedArraySeed", function(seed) new_DelayedArray(seed, Class="ReloadedArray"))

#' @export
#' @importFrom DelayedArray matrixClass
setMethod("matrixClass", "ReloadedArray", function(x) "ReloadedMatrix")

# Overrides copied from DelayedArray::ConstantArray.
#' @importFrom S4Vectors new2
setAs("ReloadedArray", "ReloadedMatrix", function(from) new2("ReloadedMatrix", from))
setAs("ReloadedMatrix", "ReloadedArray", function(from) from)

#' @export
setMethod("path", "ReloadedArraySeed", function(object, ...) object@path)

#' @export
setMethod("saveObject", "ReloadedArray", function(x, path, ReloadedArray.reuse.files="link", ...) {
    ReloadedArray.reuse.files <- match.arg(ReloadedArray.reuse.files, c("none", "copy", "link", "symlink", "relsymlink"))
    s <- x@seed
    if (ReloadedArray.reuse.files == "none") {
        x <- DelayedArray(s@seed)
        return(saveObject(x, path, ReloadedArray.reuse.files=ReloadedArray.reuse.files, ...))
    } 

    clone_duplicate(s@path, path, action=ReloadedArray.reuse.files)
    invisible(NULL)
})
