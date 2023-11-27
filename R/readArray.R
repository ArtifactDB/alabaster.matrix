#' Read high-dimensional arrays from disk
#'
#' Read arrays from on-disk formats, using the corresponding \code{\link{stageObject}} method. 
#' It should not be necessary for users to call this function manually. 
#'
#' @param info Named list containing the metadata for this array.
#' @param project Any argument accepted by the acquisition functions, see \code{?\link{acquireFile}}. 
#' By default, this should be a string containing the path to a staging directory.
#' 
#' @return A multi-dimensional object (usually a \linkS4class{DelayedMatrix}) containing the array data.
#'
#' @author Aaron Lun
#'
#' @examples
#' arr <- array(rpois(10000, 10), c(50, 20, 10))
#' dimnames(arr) <- list(
#'    paste0("GENE_", seq_len(nrow(arr))),
#'    letters[1:20],
#'    NULL
#' )
#'
#' dir <- tempfile()
#' saveObject(arr, dir)
#' readArray(dir)
#' 
#' @export
#' @import HDF5Array
readArray <- function(path, array.file.backed=TRUE, ...) {
    fpath <- file.path(path, "array.h5")

    details <- local({
        fhandle <- H5Fopen(fpath)
        on.exit(H5Fclose(fhandle), add=TRUE, after=FALSE)
        ghandle <- H5Gopen(fhandle, "dense_array")
        on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)

        type <- h5_read_attribute(ghandle, "type")
        transposed <- h5_read_attribute(ghandle, "transposed", check=TRUE, default=0L)

        dhandle <- H5Dopen(ghandle, "data")
        on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)
        placeholder <- h5_read_attribute(dhandle, "missing-value-placeholder", check=TRUE, default=NULL)

        ndims <- length(H5Sget_simple_extent_dims(H5Dget_space(dhandle)))
        names <- load_names(ghandle, ndims)

        list(type=type, names=names, transposed=(transposed != 0L), placeholder=placeholder)
    })

    out <- HDF5Array(filepath=fpath, name="dense_array/data", type=from_array_type(details$type))

    if (!is.null(details$names)) {
        dimnames(out) <- rev(details$names)
    }
    if (!details$transposed) { # All R-based HDF5 bindings will automatically transpose, so we need to untranspose if `transposed=FALSE`.
        out <- t(out)
    }
    if (!is.null(details$placeholder)) {
        out <- DelayedMask(out, placeholder=details$placeholder)
    }

    if (!array.file.backed) {
        return(as.array(out))
    } else {
        return(DelayedArray(out))
    }
}

##############################
######### OLD STUFF ##########
##############################

#' @export
loadArray <- function(info, project) {
    seed <- .createRawArraySeed(info, project=project, names=TRUE)
    DelayedArray(seed)
}
