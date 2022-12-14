% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stageArray.R
\name{stageArray}
\alias{stageArray}
\alias{stageObject,array-method}
\alias{stageObject,DelayedArray-method}
\alias{preserveDelayedOperations}
\alias{stageObject,Matrix-method}
\alias{stageObject,DelayedMatrix-method}
\title{Stage a multi-dimensional array for upload}
\usage{
\S4method{stageObject}{array}(x, dir, path, child = FALSE)

\S4method{stageObject}{DelayedArray}(x, dir, path, child = FALSE)

preserveDelayedOperations(preserve)

\S4method{stageObject}{Matrix}(x, dir, path, child = FALSE)

\S4method{stageObject}{DelayedMatrix}(x, dir, path, child = FALSE)
}
\arguments{
\item{x}{An array, almost always integer or numeric, though logical and character matrices are also supported.
Alternatively, a \linkS4class{DelayedArray} or any instance of a \linkS4class{Matrix} class.}

\item{dir}{String containing the path to the staging directory.}

\item{path}{String containing the relative path to a subdirectory inside the staging directory, in which \code{x} is to be saved.}

\item{child}{Logical scalar indicating whether \code{x} is a child of a larger object.}

\item{preserve}{Whether to preserve delayed operations using the \pkg{chihaya} specification.}
}
\value{
For the \code{stageObject} methods, the array is saved into a single file at \code{file.path(dir, path)}, possibly after appending an arbitrary file extension. 
A named list is returned, containing at least:
\itemize{
\item \code{$schema}, a string specifying the schema to use to validate the metadata.
\item \code{path}, a string containing the path to the file inside the subdirectory, containing the assay contents.
\item \code{is_child}, a logical scalar equal to the input \code{child}.
}

For \code{preserveDelayedOperations}, a logical scalar is returned indicating whether delayed operations are to be preserved by the DelayedArray method.
If \code{preserve} is supplied, it is used to set this scalar, and the \emph{previous} value of the scalar is returned.
}
\description{
Stage a high-dimensional array in preparation for upload to DataSetDB.
}
\details{
The default behavior is to save the array as a dense matrix in a HDF5 file using methods from the \pkg{HDF5Array} package.
Other representations may have more appropriate formats, which are supported by simply writing new methods for this generic.
Note that specialized methods will usually require new schemas to validate any new metadata fields.

If \code{x} itself is a child of a larger object, we suggest using the output \code{path} when referencing \code{x} from within the larger object's metadata.
This is because \code{stageObject} methods may add more path components, file extensions, etc. to the input \code{path} when saving the object.
As a result, the output \code{path} may not be the same as the input \code{path}.

By default, \code{preserveDelayedOperations()} is \code{FALSE} so any DelayedArray \code{x} will be saved as a dense HDF5 dataset.
If \code{preserveDelayedOperations()} is \code{TRUE}, DelayedArrays will instead be saved in the \pkg{chihaya} specification,
where the delayed operations are themselves stored in the HDF5 file (see \url{https://ltla.github.io/chihaya} for details).
}
\examples{
dir <- tempfile()
dir.create(dir)

mat <- array(rpois(10000, 10), c(50, 20, 10))
dimnames(mat) <- list(
   paste0("GENE_", seq_len(nrow(mat))),
   letters[1:20],
   NULL
)

path <- "whee"
stageObject(mat, dir, path)

list.files(dir)

}
\author{
Aaron Lun
}
