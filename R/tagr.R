#' tagr: A package for tagging models to estimate abundance
#'
#' The tagr package provides methods for tag-based estimates of 
#' abundance 
#' 
#' @section Single release tag recapture models:
#' Implementation of Petersen and Chapman estimators (Seber 1982)
#' along with bootstrapped confidence intervals
#'
#' @section Vignettes:
#' To learn more about tagr, start with the vignettes:
#' \code{browseVignettes(package = "tagr")}
#'
#' @docType package
#' @name tagr
NULL
#> NULL


#' Numbers of carp and those with tags captured in 2014/15
#'
#' A dataset containing fishing events for carp in Lake 
#' Sorell. The variables are as follows:
#'
#' @format A data frame with 195 rows and 3 variables:
#' \itemize{
#'   \item day: day of fishing event starting 1st July 2014
#'   \item catch: total carp caught
#'   \item tagged: number of tagged carp caught
#' }
"carphauls"

#' Tag release and recapture data from Lake Sorell
#'
#' A dataset containing tag release and recapture information for carp 
#' released in January 2012. All fish were double tagged with T-bar tags. 
#' The variables are as follows:
#'
#' @format A data frame with 803 rows and 9 variables:
#' \itemize{
#'   \item release: either first (1) or second (2)
#'   \item tags: number of tags on recaptured fish (either 1, 2), NA denotes fish still at liberty
#'   \item date: date of recapture, NA denotes fish still at liberty
#'   \item season: season (July to June) of recapture, NA denotes fish still at liberty
#' }
"carptags"
