#' Store operations in a DelayedArray
#'
#' Store the delayed operations of a \linkS4class{DelayedArray} in a HDF5 file.
#'
#' @param x Any of the delayed operation classes from \pkg{DelayedArray}.
#' @param file String containing the path to a HDF5 file.
#' @param name String containing the name of the group to save into.
#' @param ... Arguments to be passed to specific methods.
#' @return The contents of \code{x} are saved to \code{file}, and \code{NULL} is invisibly returned.
#' 
#' @author Aaron Lun
#' @examples
#' library(DelayedArray)
#' X <- DelayedArray(matrix(runif(100), ncol=20))
#' Y <- cbind(X, DelayedArray::ConstantArray(value=50, c(5, 10)))
#'
#' library(rhdf5)
#' temp <- tempfile()
#' dir.create(temp)
#'
#' fpath <- file.path(temp, "foo.h5")
#' fhandle <- H5Fcreate(fpath)
#' storeDelayedObject(Y@seed, fhandle, "YAY")
#' rhdf5::h5ls(fpath)
#' H5Fclose(fhandle)
#'
#' fhandle <- H5Fopen(fpath, "H5F_ACC_RDONLY")
#' ghandle <- H5Gopen(fhandle, "YAY")
#' reloadDelayedObject(ghandle)
#' H5Gclose(ghandle)
#' H5Fclose(fhandle)
#'
#' @aliases
#' storeDelayedObject
#' reloadDelayedObject
#' storeDelayedObject,ConstantArraySeed-method
#' storeDelayedObject,DelayedAbind-method
#' storeDelayedObject,ANY-method
#'
#' @name storeDelayedObject
NULL

r2value_type_mapping <- c(logical="BOOLEAN", integer="INTEGER", double="FLOAT", character="STRING")
to_value_type <- function(type) {
    if (!(type %in% names(r2value_type_mapping))) {
        stop("cannot map type '", type, "' to a value type")
    }
    r2value_type_mapping[[type]]
}

value2alabaster_type_mapping <- c(BOOLEAN="boolean", INTEGER="integer", FLOAT="number", STRING="string")
to_alabaster_type <- function(vtype) {
    if (!(vtype %in% names(value2alabaster_type_mapping))) {
        stop("cannot map type '", type, "' to an alabaster type")
    }
    value2alabaster_type_mapping[[vtype]]
}

#' @import alabaster.base rhdf5
load_vector_for_chihaya <- function(handle, name, version) {
    dhandle <- H5Dopen(handle, name)
    on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)
    contents <- H5Dread(dhandle, drop=TRUE) 
    if (is.raw(contents)) {
        storage.mode(contents) <- "integer"
    }

    type <- h5_read_attribute(dhandle, "type")
    type <- to_alabaster_type(type)
    missing.placeholder <- h5_read_attribute(dhandle, "missing_placeholder", check=TRUE, default=NULL)
    h5_cast(contents, expected.type=type, missing.placeholder=missing.placeholder)
}

#' @import alabaster.base rhdf5
save_vector_for_chihaya <- function(handle, name, x, version, scalar) {
    info <- transformVectorForHdf5(x)
    dhandle <- h5_write_vector(handle, name, info$transformed, scalar=scalar, emit=TRUE)
    on.exit(H5Dclose(dhandle), add=TRUE, after=FALSE)

    if (!is.null(info$placeholder)) {
        h5_write_attribute(dhandle, "missing_placeholder", info$placeholder, scalar=TRUE)
    }
    h5_write_attribute(dhandle, "type", to_value_type(type(x)), scalar=TRUE)

    invisible(NULL)
}

#######################################################
#######################################################

chihaya_array_registry <- list()
chihaya_operation_registry <- list()

#' @export
reloadDelayedObject <- function(handle, version=package_version("1.1"), ...) {
    objtype <- h5_read_attribute(handle, "delayed_type")

    if (objtype == "array") {
        arrtype <- h5_read_attribute(handle, "delayed_array")
        FUN <- chihaya_array_registry[[arrtype]]
    } else {
        optype <- h5_read_attribute(handle, "delayed_operation")
        FUN <- chihaya_operation_registry[[optype]]
    }

    FUN(handle, version=version, ...)
}

#######################################################
#######################################################

#' @export
#' @import rhdf5
setMethod("storeDelayedObject", "ConstantArraySeed", function(x, handle, name, version=package_version("1.1"), ...) {
    ghandle <- H5Gcreate(handle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)

    h5_write_attribute(ghandle, "delayed_type", "array", scalar=TRUE)
    h5_write_attribute(ghandle, "delayed_array", "constant array", scalar=TRUE)
    h5_write_vector(ghandle, "dimensions", dim(x), compress=0, type="H5T_NATIVE_UINT32")
    save_vector_for_chihaya(ghandle, "value", x@value, version=version, scalar=TRUE)
})

#' @import rhdf5 DelayedArray
chihaya_array_registry[["constant array"]] <- function(handle, version, ...) {
    dim <- h5_read_vector(handle, "dimensions")
    val <- load_vector_for_chihaya(handle, "value", version=version)
    ConstantArray(dim, value=val)
}

#######################################################
#######################################################

#' @export
#' @import rhdf5
setMethod("storeDelayedObject", "DelayedAbind", function(x, handle, name, version=package_version("1.1"), ...) {
    ghandle <- H5Gcreate(handle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)

    h5_write_attribute(ghandle, "delayed_type", "operation", scalar=TRUE)
    h5_write_attribute(ghandle, "delayed_operation", "combine", scalar=TRUE)
    h5_write_vector(ghandle, "along", x@along - 1L, type="H5T_NATIVE_UINT32", scalar=TRUE)

    shandle <- H5Gcreate(ghandle, "seeds")
    on.exit(H5Gclose(shandle), add=TRUE, after=FALSE)
    h5_write_attribute(shandle, "length", length(x@seeds), type="H5T_NATIVE_UINT32", scalar=TRUE)

    for (i in seq_along(x@seeds)) {
        storeDelayedObject(x@seeds[[i]], shandle, as.character(i - 1L), version=version, ...)
    }

    invisible(NULL)
})

#' @import alabaster.base rhdf5 DelayedArray
chihaya_operation_registry[["combine"]] <- function(handle, version, ...) {
    shandle <- H5Gopen(handle, "seeds")
    on.exit(H5Gclose(shandle), add=TRUE, after=FALSE)

    len <- h5_read_attribute(shandle, "length")
    along <- h5_read_vector(ghandle, "along")

    seeds <- vector("list", len)
    for (i in seq_len(len)) {
        x <- local({
            ihandle <- H5Gopen(shandle, as.character(i - 1L))
            on.exit(H5Gclose(ihandle))
            reloadDelayedObject(ihandle, version=version, ...)
        })
        if (!is(x, "DelayedArray")) {
            x <- DelayedArray(x)
        }
        seeds[[i]] <- x
    }

    if (along == 0L) {
        do.call(arbind, seeds)
    } else {
        do.call(acbind, seeds)
    }
}

#######################################################
#######################################################

#' @export
#' @import rhdf5
setMethod("storeDelayedObject", "ANY", function(x, handle, name, version=package_version("1.1"), ...) {
    ghandle <- H5Gcreate(handle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)

    h5_write_attribute(ghandle, "delayed_type", "array", scalar=TRUE)
    h5_write_attribute(ghandle, "delayed_array", "custom takane seed array", scalar=TRUE)

    exdir <- file.path(dirname(H5Fget_name(handle)), "seeds")
    dir.create(exdir, showWarnings=FALSE)
    n <- length(list.files(exdir))
    saveObject(x, file.path(exdir, n), ...)

    h5_write_vector(ghandle, "dimensions", dim(x), type="H5T_NATIVE_UINT32", compress=0)
    h5_write_vector(ghandle, "type", to_value_type(type(x)), scalar=TRUE)
    h5_write_vector(ghandle, "index", n, type="H5T_NATIVE_UINT32", scalar=TRUE)
})

#' @import takane.base rhdf5 DelayedArray
chihaya_array_registry[["custom takane seed array"]] <- function(handle, version, ...) {
    index <- h5_read_vector(handle, "index")
    readObject(file.path(dirname(H5Fget_name(handle)), "seeds", index), ...)
}

#######################################################
#######################################################

#' @export
#' @import rhdf5
setMethod("storeDelayedObject", "DelayedAperm", function(x, handle, name, version=package_version("1.1"), ...) {
    ghandle <- H5Gcreate(handle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)

    h5_write_attribute(ghandle, "delayed_type", "operation", scalar=TRUE)
    h5_write_attribute(ghandle, "delayed_operation", "transpose", scalar=TRUE)

    h5_write_vector(ghandle, "dimensions", x@perm - 1L, type="H5T_NATIVE_UINT32")
    storeDelayedObject(x@seed, ghandle, "seed", version=version, ...)
    invisible(NULL)
})

#' @import DelayedArray rhdf5
chihaya_operation_registry[["transpose"]] <- function(handle, version, ...) {
    shandle <- H5Gopen(handle, "seed")
    on.exit(H5Gclose(shandle), add=TRUE, after=FALSE)

    x <- reloadDelayedObject(shandle, version=version, ...)
    if (!is(x, "DelayedArray")) {
        x <- DelayedArray(x)
    }

    perm <- h5_read_vector(handle, "permutation")
    aperm(x, perm + 1L)
}

#######################################################
#######################################################

#' @export
#' @import rhdf5
setMethod("storeDelayedObject", "DelayedNaryIsoOp", function(x, handle, name, version=package_version("1.1"), ...) {
    ghandle <- H5Gcreate(handle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)
    h5_write_attribute(ghandle, "delayed_type", "operation", scalar=TRUE)

    # Figuring out the identity of the operation.
    chosen <- NULL
    for (p in chihaya.supported.Ops) {
        if (identical(x@OP, get(p, envir=baseenv()))) {
            chosen <- p
            break
        }
    }
    if (is.null(chosen)) {
        stop("unknown operation in ", class(x))
    }

    if (chosen %in% chihaya.supported.Arith) {
        h5_write_attribute(ghandle, "delayed_operation", "binary arithmetic", scalar=TRUE)
    } else if (chosen %in% chihaya.supported.Compare) {
        h5_write_attribute(ghandle, "delayed_operation", "binary comparison", scalar=TRUE)
    } else if (chosen %in% chihaya.supported.Logic) {
        h5_write_attribute(ghandle, "delayed_operation", "binary logic", scalar=TRUE)
        chosen <- chihaya_translate_Ops_for_saving(chosen)
    }
    h5_write_vector(ghandle, "method", chosen, scalar=TRUE)

    if (length(x@seeds) != 2) {
        stop("expected exactly two seeds for 'DelayedNaryIsoOp'")
    }
    if (length(x@Rargs)) {
        stop("expected no additional right arguments for 'DelayedNaryIsoOp'")
    }

    storeDelayedObject(x@seeds[[1]], ghandle, "left")
    storeDelayedObject(x@seeds[[2]], ghandle, "right")
    invisible(NULL)
})

#' @import DelayedArray
chihaya_load_binary_op <- function(handle, version, logic, ...) {
    lhandle <- H5Gopen(handle, "left")
    on.exit(H5Gclose(lhandle), add=TRUE, after=FALSE)
    left <- reloadDelayedObject(lhandle, version=version, ...)
    if (!is(left, "DelayedArray")) {
        left <- DelayedArray(left)
    }

    rhandle <- H5Gopen(handle, "right")
    on.exit(H5Gclose(rhandle), add=TRUE, after=FALSE)
    right <- reloadDelayedObject(rhandle, version=version, ...)
    if (!is(right, "DelayedArray")) {
        right <- DelayedArray(right)
    }

    op <- h5_read_vector(handle, "method")
    if (logic) {
        op <- chihaya_translate_Ops_for_loading(op)
    }
    get(op, envir=baseenv())(left, right)
}

chihaya_operation_registry[["binary arithmetic"]] <- function(handle, version, ...) chihaya_load_binary_op(handle, version, logic=FALSE, ...)
chihaya_operation_registry[["binary comparison"]] <- function(handle, version, ...) chihaya_load_binary_op(handle, version, logic=FALSE, ...)
chihaya_operation_registry[["binary logic"]] <- function(handle, version, ...) chihaya_load_binary_op(handle=handle, version=version, logic=TRUE, ...)

#######################################################
#######################################################

#' @export
#' @import rhdf5
setMethod("storeDelayedObject", "DelayedSetDimnames", function(x, handle, name, version=package_version('1.1'), ...) {
    ghandle <- H5Gcreate(handle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)

    h5_write_attribute(ghandle, "delayed_type", "operation", scalar=TRUE)
    h5_write_attribute(ghandle, "delayed_operation", "dimnames", scalar=TRUE)

    dhandle <- H5Gcreate(ghandle, "dimnames")
    on.exit(H5Gclose(dhandle), add=TRUE, after=FALSE)
    h5_write_attribute(dhandle, "length", length(x@dimnames), type="H5T_NATIVE_UINT32")

    for (i in seq_along(x@dimnames)) {
        dn <- dimnames[[i]]
        if (is.character(dn)) { # avoid -1's.
            h5_write_vector(dhandle, as.character(i - 1L), dn)
        }
    }

    storeDelayedObject(x@seed, ghandle, "seed", version=version, ...)
    invisible(NULL)
})

#' @import rhdf5 DelayedArray
chihaya_operation_registry[["dimnames"]] <- function(handle, version, ...) {
    shandle <- H5Gopen(handle, "seed")
    on.exit(H5Gclose(shandle), add=TRUE, after=FALSE)
    x <- reloadDelayedObject(shandle, version=version, ...)
    if (!is(x, "DelayedArray")) {
        x <- DelayedArray(x)
    }

    dhandle <- H5Gopen(handle, "dimnames")
    on.exit(H5Gclose(dhandle), add=TRUE, after=FALSE)
    dlen <- h5_read_attribute(dhandle, "length")
    dnames <- vector("list", dlen)
    for (i in seq_along(dnames)) {
        n <- as.character(i - 1L)
        if (h5_object_exists(dhandle, n)) {
            dnames[[i]] <- h5_read_vector(dhandle, n)
        }
    }

    dimnames(x) <- dnames
    x
}

#######################################################
#######################################################

#' @import rhdf5 alabaster.base
save_chihaya_indices <- function(handle, name, indices) { 
    ihandle <- H5Gcreate(handle, name)
    on.exit(H5Gclose(ihandle), add=TRUE, after=FALSE)
    h5_write_attribute(dhandle, "length", length(indices), type="H5T_NATIVE_UINT32")

    for (i in seq_along(indices)) {
        ii <- indices[[i]]
        if (!is.null(ii)) {
            h5_write_vector(dhandle, as.character(i - 1L), ii - 1L, type="H5T_NATIVE_UINT32")
        }
    }
}

#' @import rhdf5 alabaster.base
load_chihaya_indices <- function(handle, name) {
    ihandle <- H5Gopen(handle, name)
    on.exit(H5Gclose(ihandle), add=TRUE, after=FALSE)
    ilen <- h5_read_attribute(ihandle, "length")

    indices <- vector("list", ilen)
    for (i in seq_len(ilen)) {
        n <- as.character(i - 1L)
        if (!h5_object_exists(ihandle, n)) {
            indices[[i]] <- h5_read_vector(dhandle, n)
        }
    }

    indices
}

#' @export
#' @import rhdf5
setMethod("storeDelayedObject", "DelayedSubassign", function(x, handle, name, version, ...) {
    ghandle <- H5Gcreate(handle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)

    h5_write_attribute(ghandle, "delayed_type", "operation", scalar=TRUE)
    h5_write_attribute(ghandle, "delayed_operation", "subset assignment", scalar=TRUE)

    save_chihaya_indices(ghandle, "index", x@Lindex) 
    storeDelayedObject(x@seed, ghandle, "seed", version=version, ...)
    storeDelayedObject(x@Rvalue, ghandle, "value", version=version, ...)
    invisible(NULL)
})

#' @import rhdf5 DelayedArray
chihaya_operation_registry[["subset assignment"]] <- function(handle, version, ...) {
    shandle <- H5Gopen(handle, "seed")
    on.exit(H5Gclose(shandle), add=TRUE, after=FALSE)
    x <- reloadDelayedObject(shandle, version=version, ...)
    if (!is(x, "DelayedArray")) {
        x <- DelayedArray(x)
    }

    vhandle <- H5Gopen(handle, "value")
    on.exit(H5Gclose(vhandle), add=TRUE, after=FALSE)
    value <- reloadDelayedObject(vhandle, version=version, ...)
    if (!is(x, "DelayedArray")) {
        value <- DelayedArray(value)
    }

    indices <- load_chihaya_indices(ghandle, "index")
    do.call(`[<-`, c(list(x=x), indices, list(value=value)))
} 

#######################################################
#######################################################

#' @export
#' @import rhdf5
setMethod("storeDelayedObject", "DelayedSubset", function(x, handle, name, version, ...) {
    ghandle <- H5Gcreate(handle, name)
    on.exit(H5Gclose(ghandle), add=TRUE, after=FALSE)

    h5_write_attribute(ghandle, "delayed_type", "operation", scalar=TRUE)
    h5_write_attribute(ghandle, "delayed_operation", "subset", scalar=TRUE)

    save_chihaya_indices(ghandle, "index", x@Lindex) 
    storeDelayedObject(x@seed, ghandle, "seed", version=version, ...)
    invisible(NULL)
})

#' @import rhdf5 DelayedArray
chihaya_operation_registry[["subset"]] <- function(handle, version, ...) {
    shandle <- H5Gopen(handle, "seed")
    on.exit(H5Gclose(shandle), add=TRUE, after=FALSE)
    x <- reloadDelayedObject(shandle, version=version, ...)
    if (!is(x, "DelayedArray")) {
        x <- DelayedArray(x)
    }

    indices <- load_chihaya_indices(ghandle, "index")
    do.call(`[`, c(list(x), indices, list(drop=FALSE)))
} 
