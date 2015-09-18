## General function and transformations
##
## converstion for instantaneous rates


#' logistic transformation
#' 
#' Calculate the logit, inverse logit or inverse logit of the gradient
#' @param p parameter
#' @export
#' @aliases ilogit ilogit.gr
logit <- function(p) log(p/(1-p))

#' @export
#' @rdname logit
ilogit <- function(p) 1/(exp(-p)+1)

#' @export
#' @rdname logit
ilogit.gr <- function(p) exp(-p)/(exp(-p)+1)^2


## this will work differently for Ricker type I and type II fisheries
## we need to know what F is unfortunately

