## population size with bootstrapped uncertainty using multiple releases

#' Parameters for specifying multiple release tag model
#' 
#' Parameters for specifying multiple release tag model
#' 
#' A list that specifies the following parameters. Mixture of text, integer and
#' vectors 
#' 
#' @seealso \code{\link{multi_release}} for more details.
#'  
#' @param mean_wt mean weight of a single fish (optional argument for Chapman 
#' weight method)
#' @param method The method to use for calculating population size (options are
#' "Petersen" or "Chapman")
#' @param unit the unit of measurement (either "numbers", "kg" or "tonnes")
#' @param type does fishing occur over a short period (Ricker type 1) or is
#' extended over the year and competes with natural mortality and tag sheddding
#' (Ricker type 2) 
#' @param tag_mort vector of initial tag-induced mortality
#' @param reporting vector of annual tag reporting rates
#' @param nat_mort vector of natural mortality (instantaneous)
#' @param chronic_shed vector of chronic (ongoing) tag shedding
#' @param chronic_mort vector of chronic (ongoing) tag-induced mortality
#' @name pars
NULL
#> NULL

#' Check multiple release single recapture model inputs
#' 
#' Check multiple release single recapture model inputs
#' 
#' Check the inputs for \code{\link{multi_release}}
#' @param tags tag releases and recaptures
#' @param hauls haul data of the year to calculate the estimate
#' @param pars model parameters
#' @name check_mrelease
NULL       
#> NULL

#' @export
#' @rdname check_mrelease
check_mrelease_data <- function(tags, hauls){
  ## define the check variable
  check <- TRUE
  ## add checks
  ## return check
  check
}

#' @export
#' @rdname check_mrelease
check_mrelease_pars <- function(pars) {
  ## define the check variable
  check=TRUE
  ## add checks
  ## return check
  check
}

#' Multiple tag release estimate of population size
#' 
#' Estimate population size from multiple release, single recapture tagging data
#' 
#' For a Type 1 fishery the adjusted releases are calculate as follows
#' 
#' 1) Initial tag-induced mortality is applied
#' 
#' 2) Within season recaptures divided by the tag reporting rate are removed
#' 
#' 3) Natural mortality and chronic tag loss and tag-induced mortality is applied.
#' 
#' For a fishery that operates throughout the year (Ricker Type 2) fishing and
#' natural mortality competed to deplete the tagged population. The adjusted
#' releases are calculated as follows
#' 
#' 1) Initial tag-induced mortality is applied
#' 
#' 2) Half of the within season recaptures divided by the tag reporting rate are
#' removed 
#' 
#' 3) Half of the natural mortality and chronic tag loss and tag-induced
#' mortality is applied
#' 
#' 4) The remaining within season recaptures divided by the tag reporting rate 
#' are removed 
#' 
#' 5) The remaining natural mortality and chronic tag loss and tag-induced
#'  mortality is applied.
#' @param tags matrix (or dataframe?) where the first column specifies the annual releases and
#' subsequent columns the annual recaptures by release cohort (rows). Note 
#' these must be whole numbers
#' @param hauls matrix (or dataframe?) where the first column represents the
#' untagged recaptures and subsequent columns the recaptures by release cohort 
#' with the first (oldest) cohort to the left and the last (youngest) to the
#' right
#' @param pars list of model parameters
#' @seealso \code{\link{pars}} 
#' @export
multi_release <- function(tags, hauls, pars)  { # will perhaps add hauls
  ## check the inputs
  # check_data <- check_mrelease_data(tags, hauls, recaps, hauls)
  # ## check the parameters
  # check_pars <- check_mrelease_pars(pars)
  ## the checks will identify any problems with the inputs
  rels <- tags[,1]
  ## this needs to be safer
  recs <- tags[,-1]
  ## we'll assume all of the checks have been done
  ## create a matrix to store the available tags at the end of each year
  avail_tags <- matrix(NA, nrow=nrow(recs), ncol=ncol(recs))
  ## calculate the available tags based on Ricker fishery type
  if(pars[["type"]] == 1){
    ## calculate the available tags for each cohort (row)
    for(i in 1:nrow(avail_tags)){
      ## loop over the years
      for(j in 1:ncol(avail_tags)){
        ## tempory object to store the available tags
        temp_tags <- NULL
        ## condition based on the position of i and j
        if(j < i){
          ## do nothing - appears to work
        }else if(j==i){
          ## tag-induced mortality is applied in the release year only
          temp_tags <- rels[i] * exp(-pars[["tag_mort"]][j])
          ## remove recaptures scaled for reporting rate (could improve)
          temp_tags <- temp_tags - (recs[i,j] / pars[["reporting"]][j])
          ## apply natural mortality etc
          avail_tags[i,j] <- temp_tags*exp(-pars[["nat_mort"]][j]
                                           -pars[["chronic_shed"]][j]
                                           -pars[["chronic_mort"]][j])
        }else if(j>i){
          temp_tags <- avail_tags[i,j-1]
          ## repeat the removals above
          temp_tags <- temp_tags - (recs[i,j] / pars[["reporting"]][j])
          ## apply natural mortality etc
          avail_tags[i,j] <- temp_tags*exp(-pars[["nat_mort"]][j]
                                           -pars[["chronic_shed"]][j]
                                           -pars[["chronic_mort"]][j])
          
        }
      }
    }
  }else if(pars[["type"]]==2){
    ## calculate the available tags for each cohort (row)
    for(i in 1:nrow(avail_tags)){
      ## loop over the years
      for(j in 1:ncol(avail_tags)){
        ## tempory object to store the available tags
        temp_tags <- NULL
        ## condition based on the position of i and j
        if(j < i){
          ## do nothing 
        }else if(j==i){
          ## tag-induced mortality is applied in the release year only
          temp_tags <- rels[i] * exp(-pars[["tag_mort"]][j])
          ## remove 1/2 recaptures scaled for reporting rate (could improve)
          temp_tags <- temp_tags - 0.5*(recs[i,j] / pars[["reporting"]][j])
          ## apply 1/2 natural mortality etc
          avail_tags[i,j] <- temp_tags*exp(0.5*(-pars[["nat_mort"]][j]
                                                -pars[["chronic_shed"]][j]
                                                -pars[["chronic_mort"]][j]))
          ## remove second 1/2 recaptures and natural mortality
          temp_tags <- temp_tags - 0.5*(recs[i,j] / pars[["reporting"]][j])
          avail_tags[i,j] <- temp_tags*exp(0.5*(-pars[["nat_mort"]][j]
                                                -pars[["chronic_shed"]][j]
                                                -pars[["chronic_mort"]][j]))
        }else if(j>i){
          temp_tags <- avail_tags[i,j-1]
          ## repeat the removals above without tag-induced mortality
          temp_tags <- temp_tags - 0.5*(recs[i,j] / pars[["reporting"]][j])
          avail_tags[i,j] <- temp_tags*exp(0.5*(-pars[["nat_mort"]][j]
                                                -pars[["chronic_shed"]][j]
                                                -pars[["chronic_mort"]][j]))
          temp_tags <- temp_tags - 0.5*(recs[i,j] / pars[["reporting"]][j])
          avail_tags[i,j] <- temp_tags*exp(0.5*(-pars[["nat_mort"]][j]
                                                -pars[["chronic_shed"]][j]
                                                -pars[["chronic_mort"]][j]))
        }
      }
    }
  }else stop("either Ricker type 1 or type 2 fishery must be specified")
  ## we now have the available tags by year and would like to estimate population size
  ## cohort tags
  n_years <- nrow(hauls)-1
  ## adjust the recaptures by cohort by reporting rate in the last year
  recap_cohort <- colSums(hauls[,-1]) / pars[["reporting"]][n_years]
  ## the catch (numbers) is the total number of fish 
  catch <- sum(hauls)
  ## define storage for the cohort estimates
  ##* what is recs???
  cohort_est <- rep(NA, nrow(recs))
  ## then calculate population size based on the method 
  if(pars[["method"]]=="Petersen"){
    ## Petersen population estimate overall and by cohort
    est <- petersen(sum(avail_tags), catch, sum(recap_cohort))
    for(i in 1:nrow(recs)){
      cohort_est[i] <- petersen(avail_tags[i], catch, recap_cohort[i])
    }
  }else if(pars[["method"]]=="Chapman" & pars[["unit"]] %in% c("numbers")){
    ## Chapman population estimate overall and by cohort
    est <- chapman_n(sum(avail_tags), catch, sum(recap_cohort))
    for(i in 1:nrow(recs)){
      cohort_est[i] <- chapman_n(avail_tags[i], catch, recap_cohort[i])
    }
  }else if(pars[["method"]]=="Chapman" & pars[["unit"]] %in% c("kg", "tonnes")){
    ## Chapman weight
    est <- chapman_wt(sum(avail_tags), catch, sum(recap_cohort), pars[["mean_wt"]])
    for(i in 1:nrow(recs)){
      cohort_est[i] <- chapman_wt(sum(avail_tags), catch, sum(recap_cohort), 
                                  pars[["mean_wt"]])
    }
  }else stop("method and unit combination not available")
  ## store the data
  ## then collate the results
  ##* so this needs to be rewritten
  obj <- list("Tags" = tags, #* do we need tags?
              "Hauls" = hauls,
              "Pars" = pars,
              "Avail_tags" = avail_tags,
              "Est" = c(est, cohort_est))
  ## add class 'mrelease'
  class(obj) <- 'mrelease'
  obj
}

