getMlt <- function(mltDF, type, ...)
{
  #General interface for extracting various things from MltDataFrame
  if(class(object) != "MltDataFrame")
  {stop("Provided object is not a MltDataFrame!")}

  params = list(...)

  #By doing this we can use methods
  class(type) = type

  getMlt_internal(type, mltDF, params)
}


getMlt_internal <- function(type, mltDF, params)
{
  UseMethod("getMlt_internal")
}

getMlt_internal.transformed <- function(type, mltDF, params)
{
  #TODO: better ways to remove NA
  #Returns normal df with transformed variables
  #options:
  #dummies - factors name to create dummies or TRUE to do all
  #add.index - adds numeric index to each row
  #na.replace - with what to replace NA

  #extracting parameters
  if("dummies" %in% names(params))
    {var_dum = params[["dummies"]]} else {var_dum = FALSE}

  if("add.index" %in% names(params))
    {add.index = params[["add.index"]]} else {add.index = FALSE}

  if("na.replace" %in% names(params))
    {na.replace = params[["na.replace"]]} else {na.replace = NULL}

  #Check which factors to replace
  if(!is.character(var_dum))
    {all_dummies = ifelse(var_dum, TRUE, FALSE)} else {all_dummies = FALSE}

  #main loop
  vars = mltDF$variables

  for(i in names(vars))
  {

  }
}


