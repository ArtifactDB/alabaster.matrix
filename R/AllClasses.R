#' @export
#' @import methods
setClass("WrapperArraySeed", contains="VIRTUAL", slots=c(seed="ANY"))

#' @export
#' @importClassesFrom DelayedArray DelayedArray
setClass("WrapperArray", contains=c("VIRTUAL", "DelayedArray"), slots=c(seed="WrapperArraySeed"))

#' @export
#' @importClassesFrom DelayedArray DelayedAbind
setClass("AmalgamatedArraySeed", contains="DelayedAbind", slots=c(samples = "character"))

#' @export
#' @importClassesFrom DelayedArray DelayedArray
setClass("AmalgamatedArray", contains="DelayedArray", slots=c(seed = "AmalgamatedArraySeed"))

#' @export
#' @importClassesFrom DelayedArray DelayedMatrix
setClass("AmalgamatedMatrix", contains=c("AmalgamatedArray", "DelayedMatrix"))

#' @export
setClass("ReloadedArraySeed", contains="WrapperArraySeed", slots=c(path="character"))

#' @export
setClass("ReloadedArray", contains="WrapperArray", slots=c(seed="ReloadedArraySeed"))

#' @export
#' @importClassesFrom DelayedArray DelayedMatrix
setClass("ReloadedMatrix", contains=c("ReloadedArray", "DelayedMatrix"))
