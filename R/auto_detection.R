#The goal of auto-detection is try to distinguish variables accordingly
#Since it might be impossible, we suggest types at first and ask user
#' @export
detect_type <- function(df)
{
  #Interface for variables type auto-detection
  cols <- colnames(df)
  detected = data.frame(variable=character(), type = character())
  unsure = data.frame(variable=character(), type = character())
  result = list()
  for(i in cols)
  {
    res = detect_type_internal(df[,i])

    if(res[1] == "1")
    {
      detected[nrow(detected)+1,] = c(i, res[2])
    } else
    {
      unsure[nrow(unsure)+1,] = c(i, res[2])
    }
  }

  col_types = c()

  if(nrow(detected) != 0) {col_types = append(col_types, detected[,2])}
  if(nrow(unsure) != 0) {col_types = append(col_types, unsure[,2])}

  col_types_uni = unique(col_types)

  for(i in col_types_uni)
  {
    if(nrow(detected) != 0) {detected_i = detected[detected[,2] == i, 1]} else {detected_i = c()}
    if(nrow(unsure) != 0) {unsure_i = unsure[unsure[,2] == i, 1]} else {unsure_i = c()}

    result[[i]] = list(detected = detected_i, candidates = unsure_i)
  }
  result
}


detect_type_internal <- function(vec)
{
  #Internal function that analyze singular vector
  #As first, we can separate numeric, character and possibly other variables
  vec_nm = na.omit(vec)

  if(is.character(vec_nm))
  {
    #Then we have 3 options (date, factor or string)
    detect_type_internal_character(vec_nm)
  } else if(is.numeric(vec_nm))
  {
    detect_type_internal_numeric(vec_nm)
  } else
  {
    detect_type_internal_other(vec_nm)
  }
}

detect_type_internal_character <- function(vec_nm)
{
  #[0,1] <- it's not a date
  vec_n = length(vec_nm)
  vec_dates_p = 0
  #To avoid long computational time (12 months * 6 types of sep)
  if(length(unique(sapply(vec_nm, nchar))) < 72)
  {
    vec_dates = sapply(vec_nm, DateFormat)
    vec_dates_p = length(na.omit(vec_dates))/vec_n

    if(vec_dates_p >= 0.8)
     {return(c("1", "date"))} else if(vec_dates_p > 0.1)
     {return(c("2", "date"))}
  }

  #It might be a factor then (we consider max 20 levels)
  #If between 20 and 50 it might be a factor
  vec_ulev = length(unique(vec_nm))

  if(vec_ulev <= 20)
  {return(c("1", "factor"))} else if(vec_ulev <= 50)
  {return(c("2", "factor"))}

  #Then we check both of those condiditons above
  if((vec_dates_p > 0) || (vec_ulev <= 50))
    {return(c("2", "character"))} else {return(c("1", "character"))}
}

detect_type_internal_numeric <- function(vec_nm)
{
  vec_n = length(vec_nm)

  #We check if it's an integer type
  #It's possible that it's still a factor with weird encoding (like 1.0, 2.0, 3.0)
  #If it's the case, we try casting it as integers
  if(all(as.integer(vec_nm) == vec_nm)) {vec_nm = as.integer(vec_nm)}

  #Firstly, we check if it's only integers
  if(is.integer(vec_nm))
  {
    #We need to check if it's a numeric variable or with only natural numbers
    #or it's a factor variable
    #it's tricky since there is no good way to distinguish
    #Idea is to order unique values and check if differences between the are somehow stable
    if(areValuesInOrder(vec_nm)) {return(c("1", "factor"))}

    if(length(unique(vec_nm)) <= 20)
      {return(c("2", "factor"))} else if(length(unique(vec_nm)) <= 50)
      {return(c("2", "numeric"))} else {return(c("1", "numeric"))}

  } else
  {
    if(length(unique(vec_nm)) <= 50) {return(c("2", "numeric"))} else {return(c("1", "numeric"))}
  }
}

detect_type_internal_other <- function(vec_nm)
{
  #TODO: what other types should we consider?
  return(c("1", "other"))
}
