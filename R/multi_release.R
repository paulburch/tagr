## population size with bootstrapped uncertainty using multiple releases
##
## TODO
## function to check the pars and expand any that are the wrong length
## The current code allows the available tags to be negative
## Ricker Type 2 fishery is untested


## I've renamed the 'type' parameter in 'single_release' to 'Ricker', it's clearer

## this function now a matrix of tag releases and recaptures, catch and recaptures
check_mrelease_data <- function(tags, hauls){
  ## define the check variable
  check <- TRUE
  ## add checks
  ## check the releases are > recaptures
  # if((sum(tags) - sum(recaps)) < 0) 
  #   stop("more tagged individuals have been recaptured than were released")
  # ## check the method
  # ## so we expect to have more than one
  # if(length(catch) != length(recaps)) stop("catch and recaptures must be of the same length")
  # ## more checks as they come to mind
  # ## if catch and recaps are length 1 we can't bootstrap,
  # ## however this isn't necessarily a problem
  # ## define the number of years and check vector lengths
  # n_years <- length(prior_recaps)
  ## return check
  check
}

#' Check specified parameters
#' 
#' Check specified parameters
check_mrelease_pars <- function(pars) {
  ## define the check variable
  check=TRUE
  ## add checks
  ## check the method
  # if(!method %in% c("Petersen", "Chapman")) 
  #   stop("incorrect method, currently 'Petersen' and 'Chapman' are implemented")
  # ## check units
  # if(!unit %in% c("numbers", "kg", "tonnes")) 
  #   stop("incorrect units must be numbers, kg or tonnes")
  
  ## return check
  check
}


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
#' @param tags matrix that specifies the vector of the annual numbers of marked animals released (note 
#' these must be whole numbers)
#' @param catch - these are by haul!! vector of the number or weight of animals
#'  captured and checked 
#' for tags by haul on the second survey
#' @param pars list of model parameters
#' @seealso \code{\link{pars}} 
#' @export
multi_release <- function(tags, catch, pars)  { # will perhaps add hauls
  ## check the inputs
  # check_data <- check_mrelease_data(tags, catch, recaps, hauls)
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
  
  ## define the assessment year
  assess_year <- pars[["assess_year"]]
  ## define storage for the cohort estimates, no need to specify a
  cohort_est <- rep(NA, nrow(recs))

  ## then calculate population size based on the method 
  ##* these need to be checked and corrected
  if(method=="Petersen"){
    ## estimate population size for oldest cohort first
    ## are the recaptures adjusted?
    est <- petersen(sum(avail_tags), sum(catch), sum(adj_recaps))
    for(i in 1:nrow(recs)){
      cohort_est[i] <- petersen(avail_tags[i], QQcatch, recs[i])
    }
  }else if(method=="Chapman" & unit %in% c("numbers")){
    ## Chapman numbers
    est <- chapman_n(avail_tags, sum(catch), adj_recaps)
    for(i in 1:nrow(recs)){
      cohort_est[i] <- chapman_n(avail_tags[i], QQcatch, recs[i])
    }
  }else if(method=="Chapman" & unit %in% c("kg", "tonnes")){
    ## Chapman weight
    est <- chapman_wt(avail_tags, sum(catch), adj_recaps, mean_wt)
    for(i in 1:nrow(recs)){
      cohort_est[i] <- chapman_wt(avail_tags[i], QQcatch, recs[i])
    }
  }else stop("method and unit combination not available")
  ## store the data
  ## then collate the results
  obj <- list("Tags" = tags,
              "Catch" =catch,
              "Params" = params,
              "Estimate" = est,
              "Residual" = cohort_est - est)
  ## return the results
  obj
}
