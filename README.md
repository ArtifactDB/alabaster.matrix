# Save array-like objects to file

The **alabaster.matrix** package implements methods for saving and loading matrix- or array-like objects under the **alabaster** framework.
It provides a language-agnostic method for serializing data in arrays or abstractions thereof.
To get started, install the package and its dependencies from GitHub:

```r
devtools::install_github("ArtifactDB/alabaster.schemas")
devtools::install_github("ArtifactDB/alabaster.base")
devtools::install_github("ArtifactDB/alabaster.matrix")
```

A sparse matrix can be saved to a HDF5 file in compressed sparse column format:

```r
library(Matrix)
y <- rsparsematrix(1000, 100, density=0.05)

library(alabaster.matrix)
tmp <- tempfile()
dir.create(tmp)
meta <- stageObject(y, tmp, "sparse")
meta[["$schema"]]
## [1] "hdf5sparse_matrix/v1.json"

library(alabaster.base)
roundtrip <- loadObject(meta, tmp)
## [1] "H5SparseMatrix"
## attr(,"package")
## [1] "HDF5Array"
```

We can also handle [`DelayedArray`](https://bioconductor.org/packages/DelayedArray) objects, possibly with preservation of delayed operations.
That said, it is probably best to avoid preserving delayed operations for file-backed `DelayedArray`s if you want the artifacts to be re-usable on different filesystems.

```r
library(DelayedArray)
y <- DelayedArray(rsparsematrix(1000, 100, 0.05))
y <- log1p(abs(y) / 1:100) # adding some delayed ops.

# Default method saves without preserving delayed operations.
library(alabaster.matrix)
tmp <- tempfile()
dir.create(tmp)
meta <- stageObject(y, tmp, "simple")
meta[["$schema"]]
## [1] "hdf5_sparse_matrix/v1.json"

old <- preserveDelayedOperations(TRUE)
meta <- stageObject(y, tmp, "delayed")
meta[["$schema"]]
## [1] "hdf5_delayed_array/v1.json"

library(alabaster.base)
roundtrip <- loadObject(meta, tmp)
## [1] "DelayedMatrix"
## attr(,"package")
## [1] "DelayedArray"

preserveDelayedOperations(old)
``` 
