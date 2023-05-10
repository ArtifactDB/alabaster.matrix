#' @export
#' @import methods
setClass("WrapperArraySeed", contains="VIRTUAL", slots=c(seed="ANY"))

#' @export
#' @importClassesFrom DelayedArray DelayedAbind
setClass("MultiSampleMatrixSeed", contains="DelayedAbind", slots=c(samples = "character"))

#' @export
#' @importClassesFrom DelayedArray DelayedMatrix
setClass("MultiSampleMatrix", contains="DelayedMatrix", slots=c(seed = "MultiSampleMatrixSeed"))
