#'@export
analyze_cor_basic <- function(var1, var2)
{
  #Most basic correlation between 2 variables
  #It's not intended for ts and panel variables
  #Correlation is calculated without missings
  #It's a interface for specific functions
  #Returns name of method, p-value and correlation coefficient

  tvar1 = var1$type
  tvar2 = var2$type

  raw_var1 = var1$raw
  raw_var2 = var2$raw

  #First we check if there were provided only numeric/factor variables
  if(!all(c(tvar1, tvar2) %in% c("numeric", "factor")))
  {stop("Correlation cannot be calculated between non-numeric/factor variables!")}

  #Then we check if provided variables match their real types
  if(!(isRealType(raw_var1, tvar1) & isRealType(raw_var2, tvar2)))
  {stop("Provided variable type does not match it's real type!")}

  if(all(c(tvar1, tvar2) == "numeric"))
  {
    analyze_cor_basic_num(var1, var2)
  }
}

analyze_cor_basic_num <- function(var1, var2)
{
  #Correlation between 2 numeric variables

}
