## check function inputs

## investigate further http://adv-r.had.co.nz/Exceptions-Debugging.html#condition-handling
check_srelease_inputs <- function(tags, catch, recaps){
  ## define the check variable
  check <- TRUE
  ## check if any inputs are < 0
  if(any(tags<=0, catch<=0, recaps<0)){
    check <- FALSE
    warning("releases, catch and recaptures must not be negative")
  }
  ## check if recaptures are greater than releases
  if(tags < recaps){ 
    check <- FALSE
    warning("more tags have been recaptured than were released")
  }
  ## check is recaptures are greater than catch
  if(catch < recaps){
    check <- FALSE
    warning("more tags have been recaptured than were released")
  }
  ## if there are no recaptures we don't estimate population size, or alternately it could be infinity
  if(recaps==0){
    check <- FALSE
    warning("there are zero recapures population size not estimated")
    ## now check that the adjusted releases are > 0
  }
  if(tags <= 0){
    ## alternately N_hat could be zero
    check <- FALSE
    warning("there are zero releases population size not estimated")
  }
  ## return check
  check
}
