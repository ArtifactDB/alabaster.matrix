aggregate_range <- function(collated, name) {
    range(unlist(lapply(collated, function(y) y[[name]])))
}

aggregate_any <- function(collated, name) {
    any(vapply(collated, function(y) y[[name]], TRUE))
}

###################################################
###################################################

setGeneric("collect_integer_attributes", function(x) standardGeneric("collect_integer_attributes"))

setMethod("collect_integer_attributes", "array", function(x) {
    list(
        range=suppressWarnings(range(x, na.rm=TRUE)),
        missing=anyNA(x)
    )
})

setMethod("collect_integer_attributes", "ANY", function(x) {
    collated <- blockApply(x, collect_integer_attributes)
    list(
        range=aggregate_range(collated, "range"),
        missing=aggregate_any(collated, "missing")
    )
})

optimize_integer_storage <- function(x) {
    attr <- collect_integer_attributes(x)

    if (attr$missing) {
        lower <- attr$range[1]
        upper <- attr$range[2]
        if (is.infinite(lower)) {
            return(list(type="H5T_NATIVE_INT8", placeholder=-2L^7L))
        }

        if (lower < 0L) {
            if (lower > -2^7 && upper < 2^7) {
                return(list(type="H5T_NATIVE_INT8", placeholder=-2L^7L))
            } else if (lower > -2^15 && upper < 2^15) {
                return(list(type="H5T_NATIVE_INT16", placeholder=-2L^15L))
            }
        } else {
            if (upper < 2^8 - 1) {
                return(list(type="H5T_NATIVE_UINT8", placeholder=2L^8L-1L))
            } else if (upper < 2^16 - 1) {
                return(list(type="H5T_NATIVE_UINT16", placeholder=2L^16L-1L))
            }
        }

        return(list(type="H5T_NATIVE_INT32", placeholder=NA_integer_))

    } else {
        lower <- attr$range[1]
        upper <- attr$range[2]
        if (is.infinite(lower)) {
            return(list(type="H5T_NATIVE_INT8", placeholder=NULL))
        }

        if (lower < 0L) {
            if (lower >= -2^7 && upper < 2^7) {
                return(list(type="H5T_NATIVE_INT8", placeholder=NULL))
            } else if (lower >= -2^15 && upper < 2^15) {
                return(list(type="H5T_NATIVE_INT16", placeholder=NULL))
            }
        } else {
            if (upper < 2^8) {
                return(list(type="H5T_NATIVE_UINT8", placeholder=NULL))
            } else if (upper < 2^16) {
                return(list(type="H5T_NATIVE_UINT16", placeholder=NULL))
            }
        }

        return(list(type="H5T_NATIVE_INT32", placeholder=NULL))
    }
}

###################################################
###################################################

setGeneric("collect_float_attributes", function(x) standardGeneric("collect_float_attributes"))

setMethod("collect_float_attributes", "array", collect_float_attributes)

setMethod("collect_float_attributes", "ANY", function(x) {
    collated <- blockApply(x, collect_float_attributes)

    output <- list(range=aggregate_range(collated, "range"))
    for (n in c("missing", "specials", "non_integer", "has_NaN", "has_Inf", "has_nInf", "has_lowest", "has_highest")) {
        output[[n]] <- aggregate_any(collated, n)
    }

    output
})

optimize_float_storage <- function(x) {
    attr <- collect_float_attributes(x)

    if (attr$missing) {
        if (!attr$non_integer) {
            lower <- attr$range[1]
            upper <- attr$range[2]
            if (lower < 0L) {
                if (lower > -2^7 && upper < 2^7) {
                    return(list(type="H5T_NATIVE_INT8", placeholder=-2^7))
                } else if (lower > -2^15 && upper < 2^15) {
                    return(list(type="H5T_NATIVE_INT16", placeholder=-2^15))
                } else if (lower > -2^31 && upper < 2^31) {
                    return(list(type="H5T_NATIVE_INT32", placeholder=-2^31))
                }
            } else {
                if (upper < 2^8-1) {
                    return(list(type="H5T_NATIVE_UINT8", placeholder=2^8-1))
                } else if (upper < 2^16-1) {
                    return(list(type="H5T_NATIVE_UINT16", placeholder=2^16-1))
                } else if (upper < 2^32-1) {
                    return(list(type="H5T_NATIVE_UINT32", placeholder=2^32-1))
                }
            }
        }

        placeholder <- NULL
        if (!attr$has_NaN) {
            placeholder <- NaN
        } else if (!attr$has_Inf) {
            placeholder <- Inf
        } else if (!attr$has_nInf) {
            placeholder <- -Inf
        } else if (!attr$has_lowest) {
            placeholder <- lowest_double()
        } else if (!attr$has_max) {
            placeholder <- max_double()
        }
        if (is.null(placeholder)) {
            warning("cannot guess a suitable missing value placeholder, treating all NAs as NaNs")
        }
        return(list(type="H5T_NATIVE_DOUBLE", placeholder=placeholder))

    } else {
        if (!attr$non_integer) {
            lower <- attr$range[1]
            upper <- attr$range[2]
            if (lower < 0L) {
                if (lower >= -2^7 && upper < 2^7) {
                    return(list(type="H5T_NATIVE_INT8", placeholder=NULL))
                } else if (lower >= -2^15 && upper < 2^15) {
                    return(list(type="H5T_NATIVE_INT16", placeholder=NULL))
                } else if (lower >= -2^31 && upper < 2^31) {
                    return(list(type="H5T_NATIVE_INT32", placeholder=NULL))
                }
            } else {
                if (upper < 2^8) {
                    return(list(type="H5T_NATIVE_UINT8", placeholder=NULL))
                } else if (upper < 2^16) {
                    return(list(type="H5T_NATIVE_UINT16", placeholder=NULL))
                } else if (upper < 2^32) {
                    return(list(type="H5T_NATIVE_UINT32", placeholder=NULL))
                }
            }
        }

        return(list(type="H5T_NATIVE_DOUBLE", placeholder=NULL))
    }
}
