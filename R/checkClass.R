#' 
#' @title Checks that an object has the same class in all studies
#' @description This is an internal function.
#' @details In DataSHIELD an object included in analysis must be of the same type in all 
#' the collaborating studies. If that is not the case the process is stopped
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param obj a string character, the name of the object to check for.
#' @keywords internal
#' @return a message or the class of the object if the object has the same class in all studies.
#'
checkClass <- function(datasources=NULL, obj=NULL){
  # check the class of the input object
  cally <- paste0("class(", obj, ")")
  objtypes <- opal::datashield.aggregate(datasources, cally)

  index       <- 1
  sameContent <- TRUE
  while (index < length(objtypes))
  {
    sameContent <- sameContent && (length(setdiff(objtypes[[index]], objtypes[[index + 1]])) == 0)
    index <- index + 1
  }

  if(! sameContent){
    message("The input data is not of the same class in all studies!")
    message("Use the function 'ds.class' to verify the class of the input object in each study.")
    stop(" End of process!", call.=FALSE)
  }else{
    return(unique(unlist(objtypes)))
  }
}
