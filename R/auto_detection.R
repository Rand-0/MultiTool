#The goal of autodetecion is try to distinguish variables accoridingly
#Since it might be impossible, we suggest types at first and ask user
#' @export
detect_type_internal <- function(vec)
{
  #Internal function that analyze singular vector
  #As first, we can separate numeric, character and possibly other variables
  vec_nm = na.omit(vec)

  if(is.character(vec))
  {
    #Then we have 3 options (date, factor or string)
    detect_type_internal_character(vec)
  } else if(is.numeric(vec))
  {
    detect_type_internal_numeric(vec)
  } else
  {
    detect_type_internal_other(vec)
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

  #It might be a factor then (since it's not numeric we can consider max 10% levels)
  #If between 10 and 20% it might be a vector
  vec_ulev = length(unique(vec_nm))

  if(vec_ulev <= (vec_n)/10)
  {return(c("1", "factor"))} else if(vec_ulev <= vec_n/5)
  {return(c("2", "factor"))}

  #Then we check both of those condiditons above
  if((vec_dates_p > 0) || (vec_ulev <= vec_n/2))
    {return(c("2", "character"))} else {return(c("1", "character"))}
}

detect_type_internal_numeric <- function(vec_nm)
{
  vec_n = length(vec_nm)

  #Firstly, we check if it's only integers
  if(is.integer(vec_nm))
  {
    #We need to check if it's a numeric variable or with only natural numbers
    #or it's a factor variable
    #it's tricky since there is no good way to distinguish
    #Idea is to order unique values and check if differences between the are somehow stable
    if(length(unique(vec)) <= vec_n/20)
    { vector_analysis.factor(as.factor(vec)) } else {vector_analysis.numeric(vec)}
  }
}

detect_type_internal_other <- function(vec_nm)
{

}
